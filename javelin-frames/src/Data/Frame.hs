{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  $header
-- Copyright   :  (c) Laurent P. René de Cotret
-- License     :  MIT
-- Maintainer  :  laurent.decotret@outlook.com
-- Portability :  portable
-- Stability   :  experimental
--
-- This is an experimental interface to dataframes.
--
-- This module defines the type machinery and some functions to
-- process data frames. Data frames are structures where every
-- row corresponds to an object, but data is stored in
-- contiguous arrays known as columns.
--
-- A user guide is provided in the "Data.Frame.Tutorial" module.

module Data.Frame (
    -- * Defining dataframe types
    Column, Frameable, Row, Frame,

    -- * Construction and deconstruction
    fromRows, toRows, fields,

    -- * Operations on rows
    null, length, mapRows, mapRowsM, filterRows, foldlRows,
    -- ** Sorting rows in frames
    sortRowsBy, sortRowsByUnique, 
    sortRowsByKey, sortRowsByKeyUnique, sortRowsByKeyUniqueOn,

    -- * Displaying frames
    display,
    -- ** Customizing the display of frames
    displayWith, DisplayOptions(..), defaultDisplayOptions, 

    -- * Indexing operations
    -- ** Based on integer indices
    ilookup, iat,
    -- ** Based on indexable frames
    Indexable(Key, index), lookup, at,

    -- * Merging dataframes
    -- ** Zipping rows in order
    zipRowsWith,
    -- ** Merging using an index
    mergeWithStrategy, mergeWithStrategyOn, matchedStrategy,
    -- *** Helpers to define your own merge strategies
    These(..),
) where


import Control.Exception (assert)
import Control.Monad.ST ( runST )
import Data.Bifunctor (second)
import qualified Data.Foldable
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import qualified Data.List as List ( intersperse, foldl' )
import Data.Maybe (catMaybes)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Semigroup (Max(..))
import qualified Data.Set as Set
import Data.These (These(..))
import Data.Tuple (swap)
import Data.Vector (Vector)
import qualified Data.Vector
import qualified Data.Vector.Algorithms.Tim as TimSort (sortBy, sortUniqBy)
import Prelude hiding (lookup, null, length)
import qualified Prelude
import GHC.Generics ( Selector, Generic(..), S, D, C, K1(..), Rec0, M1(..), type (:*:)(..), selName )


-- $setup
-- >>> import qualified Data.Vector as Vector

-- | Build a dataframe from a container of rows.
--
-- For the inverse operation, see `toRows`.
fromRows :: (Frameable t, Foldable f)
         => f (Row t)
         -> Frame t
fromRows = pack . Data.Vector.fromList . Data.Foldable.toList
{-# INLINE[~2] fromRows #-}


-- | Deconstruct a dataframe into its rows.
--
-- For the inverse operation, see `fromRows`.
toRows :: Frameable t 
       => Frame t
       -> Vector (Row t)
toRows = unpack
{-# INLINE[~2] toRows #-}


-- TODO: Chaining operations such as `mapRows` and `filterRows`
--       should benefit from optimizing as `toRows . fromRows = id`
--       ( and `fromRows . toRows = id` as well).
--       See the rules below.
--       It's not clear if I'm using the rewrite system correctly,
--       by looking at the benchmark resuylts
{-# RULES
"fromRows/toRows" [2] fromRows . toRows = id
"toRows/fromRows" [2] toRows . fromRows = id 
  #-}

-- | Returns `True` if a dataframe has no rows.
null :: Frameable t
     => Frame t
     -> Bool
-- TODO: we can use yet another typeclass deriving
-- from generic to only look at ONE of the columns,
-- rather than reconstructing the first row
null = Data.Vector.null . toRows


-- | Access the length of a dataframe, i.e. the number of rows.
length :: Frameable t
       => Frame t
       -> Int
-- TODO: we can use yet another typeclass deriving
-- from generic to only look at ONE of the columns,
-- rather than reconstructing all rows.
length = Data.Vector.length . toRows


-- | Map a function over each row individually.
--
-- For mapping with a monadic action, see `mapRowsM`.
mapRows :: (Frameable t1, Frameable t2)
        => (Row t1 -> Row t2)
        -> Frame t1
        -> Frame t2
mapRows f = fromRows 
           . Data.Vector.map f 
           . toRows


-- | Map each element of a dataframe to a monadic action, evaluate
-- these actions from left to right, and collect the result
-- in a new dataframe.
--
-- For mapping without a monadic action, see `mapRows`.
mapRowsM :: (Frameable t1, Frameable t2, Monad m)
         => (Row t1 -> m (Row t2))
         -> Frame t1
         -> m (Frame t2)
mapRowsM f = fmap fromRows
            . Data.Vector.mapM f
            . toRows


-- | Filter rows from a @`Frame` t@, only keeping
-- the rows where the predicate is `True`.
filterRows :: (Frameable t)
           => (Row t -> Bool)
           -> Frame t
           -> Frame t
filterRows f = fromRows 
              . Data.Vector.filter f
              . toRows


-- | Zip two frames together using a combination function.
-- Rows from each frame are matched in order; the resulting
-- frame will only contain as many rows as the shortest of
-- the two input frames
zipRowsWith :: (Frameable t1, Frameable t2, Frameable t3)
              => (Row t1 -> Row t2 -> Row t3)
              -> Frame t1
              -> Frame t2
              -> Frame t3
zipRowsWith f xs ys 
    = fromRows 
    $ Data.Vector.zipWith f 
                          (toRows xs)
                          (toRows ys)


-- | Left-associative fold of a structure, with strict application of the operator.
foldlRows :: Frameable t
          => (b -> Row t -> b) -- ^ Reduction function that takes in individual rows
          -> b                 -- ^ Initial value for the accumulator
          -> Frame t           -- ^ Data frame
          -> b
foldlRows f start 
    = Data.Vector.foldl' f start . toRows


-- | Access a row from a dataframe by its integer index. Indexing
-- starts at 0, representing the first row.
--
-- If the index is larger than the number of rows, this function
-- returns `Nothing`.
--
-- To access a specific row AND column, `iat` is much more efficient.
--
-- To lookup a row based on a non-integer index, see `lookup`.
ilookup :: Frameable t
        => Int
        -> Frame t
        -> Maybe (Row t)
ilookup = iindex


-- | Sort the rows of a frame using a custom comparison function.
--
-- Use the function `on` from "Data.Function" to easily create 
-- comparison functions. See the example below. 
--
-- If you wish to prune rows with duplicates, see `sortRowsByUnique`. 
-- If your dataframe has an instance of `Indexable`, see `sortRowsByKey`.
--
-- For example, let's say we want to sort
-- a dataframe of students by their first name:
-- 
-- >>> :{
--      data Student f
--          = MkStudent { studentName      :: Column f String
--                      , studentAge       :: Column f Int
--                      , studentMathGrade :: Column f Char
--                      }
--          deriving (Generic, Frameable)
--      students = fromRows 
--               [ MkStudent "Erika" 13 'D'
--               , MkStudent "Beatrice" 13 'B'
--               , MkStudent "David" 13 'A'
--               , MkStudent "Albert" 12 'C'
--               , MkStudent "Frank" 11 'C'
--               , MkStudent "Clara" 12 'A'
--               ]
-- :}
--
-- >>> import Data.Function (on)
-- >>> putStrLn $ display $ sortRowsBy (compare `on` studentName) students
-- studentName | studentAge | studentMathGrade
-- ----------- | ---------- | ----------------
--    "Albert" |         12 |              'C' 
--  "Beatrice" |         13 |              'B'
--     "Clara" |         12 |              'A'
--     "David" |         13 |              'A'
--     "Erika" |         13 |              'D'
--     "Frank" |         11 |              'C'
--
-- The underlying sorting algorithm is timsort (via 
-- `Data.Vector.Algorithms.Tim.sortBy`), which minimizes the number 
-- of comparisons used.
sortRowsBy :: Frameable t
           => (Row t -> Row t -> Ordering)
           -> Frame t
           -> Frame t
sortRowsBy cmp df
    = let rs = toRows df 
       in fromRows $ runST $ do
        mutVec <- Data.Vector.thaw rs
        TimSort.sortBy cmp mutVec
        Data.Vector.freeze mutVec <&> Data.Vector.force
{-# INLINABLE sortRowsBy #-}


-- | Sort the rows of a frame using a custom comparison function.
--
-- Use the function `on` from "Data.Function" to easily create 
-- comparison functions. See the example below. 
--
-- If your dataframe has an instance of `Indexable`, see `sortRowsByKey`.
--
-- For example, let's say we want to sort
-- a dataframe of students by their first name:
-- 
-- >>> :{
--      data Student f
--          = MkStudent { studentName      :: Column f String
--                      , studentAge       :: Column f Int
--                      , studentMathGrade :: Column f Char
--                      }
--          deriving (Generic, Frameable)
--      students = fromRows 
--               [ MkStudent "Erika" 13 'D'
--               , MkStudent "Beatrice" 13 'B'
--               , MkStudent "David" 13 'A'
--               , MkStudent "Albert" 12 'C'
--               , MkStudent "Frank" 11 'C'
--               , MkStudent "Clara" 12 'A'
--               ]
-- :}
--
-- >>> import Data.Function (on)
-- >>> putStrLn $ display $ sortRowsBy (compare `on` studentName) students
-- studentName | studentAge | studentMathGrade
-- ----------- | ---------- | ----------------
--    "Albert" |         12 |              'C' 
--  "Beatrice" |         13 |              'B'
--     "Clara" |         12 |              'A'
--     "David" |         13 |              'A'
--     "Erika" |         13 |              'D'
--     "Frank" |         11 |              'C'
--
-- The underlying sorting algorithm is timsort (via 
-- `Data.Vector.Algorithms.Tim.sortBy`), which minimizes the number 
-- of comparisons used.
sortRowsByUnique :: Frameable t
           => (Row t -> Row t -> Ordering)
           -> Frame t
           -> Frame t
sortRowsByUnique cmp df
    = let rs = toRows df 
       in fromRows $ runST $ do
        mutVec <- Data.Vector.thaw rs
        TimSort.sortUniqBy cmp mutVec >>= Data.Vector.freeze <&> Data.Vector.force
{-# INLINABLE sortRowsByUnique #-}


-- | Sort the rows of a frame using the index defined by
-- the `Indexable` typeclass. 
--
-- If your dataframe does not have an instance of `Indexable`, 
-- see `sortRowsBy`.
--
-- To prune rows with duplicate keys, see `sortRowsByKeyUnique`.
-- 
-- For example:
-- 
-- >>> :{
--      data Student f
--          = MkStudent { studentName      :: Column f String
--                      , studentAge       :: Column f Int
--                      , studentMathGrade :: Column f Char
--                      }
--          deriving (Generic, Frameable)
--      instance Indexable Student where
--          type Key Student = String
--          index = studentName
--      students = fromRows 
--               [ MkStudent "Erika" 13 'D'
--               , MkStudent "Beatrice" 13 'B'
--               , MkStudent "David" 13 'A'
--               , MkStudent "Albert" 12 'C'
--               , MkStudent "Frank" 11 'C'
--               , MkStudent "Clara" 12 'A'
--               ]
-- :}
--
-- >>> import Data.Function (on)
-- >>> putStrLn $ display $ sortRowsByKey students
-- studentName | studentAge | studentMathGrade
-- ----------- | ---------- | ----------------
--    "Albert" |         12 |              'C' 
--  "Beatrice" |         13 |              'B'
--     "Clara" |         12 |              'A'
--     "David" |         13 |              'A'
--     "Erika" |         13 |              'D'
--     "Frank" |         11 |              'C'
--
-- The underlying sorting algorithm is timsort (via 
-- `Data.Vector.Algorithms.Tim.sortBy`), which minimizes the number 
-- of comparisons used.
sortRowsByKey :: (Indexable t)
              => Frame t
              -> Frame t
sortRowsByKey df =
    -- I had trouble defining a method whereby one could either
    -- build a vector of keys from a `Frame` (without converting to rows), 
    -- or extract a key from a single `Row`. See "NOTE: Indexable key and index" below
    --
    -- Instead, we extract the index vector, sort it while keeping track
    -- of the initial integer positions, and finally backpermuting.
    let ix = Data.Vector.map swap 
           $ Data.Vector.indexed (index df)
        -- TODO: is it possible to run `Data.Vector.map snd` 
        -- within the `ST` context?
        sortedIx = Data.Vector.map snd $ runST $ do
            mutVec <- Data.Vector.thaw ix
            TimSort.sortBy (compare `on` fst) mutVec

            Data.Vector.freeze mutVec <&> Data.Vector.force
     in fromRows $ Data.Vector.backpermute (toRows df) sortedIx --  sortRowsBy (compare `on` index)
{-# INLINABLE sortRowsByKey #-}


-- | Sort the rows of a frame using the index defined by
-- the `Indexable` typeclass, but prune rows with duplicate keys.
--
-- The underlying sorting algorithm is timsort (via 
-- `Data.Vector.Algorithms.Tim.sortBy`), which minimizes the number 
-- of comparisons used.
sortRowsByKeyUnique :: (Indexable t)
                    => Frame t
                    -> Frame t
sortRowsByKeyUnique = sortRowsByKeyUniqueOn id


-- | Sort the rows of a frame by mapping the index defined by
-- the `Indexable` typeclass, to another key type @k@. 
-- Also prune rows with duplicate keys.
--
-- The underlying sorting algorithm is timsort (via 
-- `Data.Vector.Algorithms.Tim.sortBy`), which minimizes the number 
-- of comparisons used.
sortRowsByKeyUniqueOn :: (Ord k, Indexable t)
                      => (Key t -> k)
                      -> Frame t
                      -> Frame t
sortRowsByKeyUniqueOn mapkey df =
    -- I had trouble defining a method whereby one could either
    -- build a vector of keys from a `Frame` (without converting to rows), 
    -- or extract a key from a single `Row`.
    --
    -- Instead, we extract the index vector, sort it while keeping track
    -- of the initial integer positions, and finally backpermuting.
    let ix = Data.Vector.map swap 
           $ Data.Vector.indexed (Data.Vector.map mapkey $ index df)
        -- TODO: is it possible to run `Data.Vector.map snd` 
        -- within the `ST` context?
        sortedIx = Data.Vector.map snd $ runST $ do
            mutVec <- Data.Vector.thaw ix
            TimSort.sortUniqBy (compare `on` fst) mutVec >>= Data.Vector.freeze <&> Data.Vector.force
     in fromRows $ Data.Vector.backpermute (toRows df) sortedIx --  sortRowsBy (compare `on` index)
{-# INLINABLE sortRowsByKeyUniqueOn #-}


-- | Look up a row in a data frame by key. The specific key
-- is defined by the `Indexable` instance of type @t@.
--
-- The first row whose index matches the supplied key is 
-- returned. If no row has a matching key, returns `Nothing`.
--
-- If you need to look up a particular row and column, 
-- `at` is much more efficient.
--
-- To lookup a row based on an integer index, see `ilookup`.
lookup :: (Indexable t)  
       => Key t
       -> Frame t
       -> Maybe (Row t)
lookup key fr 
    = Data.Vector.findIndex (==key) (index fr) 
    >>= flip ilookup fr


-- | Lookup an element of a frame by row and column.
--
-- This is much more efficient than looking up an entire row 
-- using `lookup`, and then selecting a specific field from a row.
--
-- To lookup an element by integer row index instead, see `iat`.
at :: (Indexable t)
   => Frame t 
   -> (Key t, Frame t -> Vector a)
   -> Maybe a
fr `at` (row, col) 
    = Data.Vector.findIndex (==row) (index fr)
    >>= \ix -> (col fr) Data.Vector.!? ix


-- | Lookup an element of the frame by row index and column
--
-- This is much more efficient than looking up an entire row 
-- using `ilookup`, and then selecting a specific field from a row.
--
-- To lookup an element by row key instead, see `at`.
iat :: Frame t 
    -> (Int, Frame t -> Vector a)
    -> Maybe a
fr `iat` (rowIx, col) = (col fr) Data.Vector.!? rowIx


-- | Merge two dataframes using a merging strategy, where the indexes
-- of the dataframes have the same type. See `mergeWithStrategyOn`
-- to merge dataframes with different indexes.
--
-- A merging strategy handles the possibility of rows missing in the 
-- left and/or right dataframes. Merge strategies can be user-defined,
-- or you can use predefined strategies (e.g. `matchedStrategy`).
--
-- Note that (@`Key` t1 ~ `Key` t2@) means that the type of keys in
-- in both dataframes must be the same.
--
-- In the example below, we have two dataframes: one containing
-- store names, and one containing addresses. Both dataframes
-- have use a unique identification number to relate their data
-- to specific stores.
--
-- We want to build a summary of information about stores,
-- containing each store's name and address.
--
-- >>> :{
--      data Store f
--          = MkStore { storeId   :: Column f Int
--                    , storeName :: Column f String
--                    }
--          deriving (Generic, Frameable)
--      instance Indexable Store where
--          type Key Store = Int
--          index = storeId
-- :}
--
-- >>> :{
--      data Address f
--          = MkAddress { addressStoreId     :: Column f Int
--                      , addressCivicNumber :: Column f Int
--                      , addressStreetName  :: Column f String
--                      }
--          deriving (Generic, Frameable)
--      instance Show (Row Address) where
--          show (MkAddress _ civicNum streetName) = mconcat [show civicNum, " ", streetName]
--      instance Indexable Address where
--          type Key Address = Int
--          index = addressStoreId
-- :}
--
-- >>> :{
--      data StoreSummary f
--          = MkStoreSummary { storeSummaryName    :: Column f String
--                           , storeSummaryAddress :: Column f (Row Address)
--                           }
--          deriving (Generic, Frameable)
--      deriving instance Show (Row StoreSummary)
-- :}
--
-- >>> :{
--     stores = fromRows 
--              [ MkStore 1 "Maxi"
--              , MkStore 2 "Metro"
--              , MkStore 3 "Sobeys"
--              , MkStore 4 "Loblaws"
--              ]
-- :}
--
-- >>> :{
--     addresses = fromRows
--                 [ MkAddress 1 1982 "14th Avenue"
--                 , MkAddress 2 10   "Main Street"
--                 , MkAddress 3 914  "Prima Street"
--                 -- Missing address for store id 4
--                 , MkAddress 5 1600 "Cosgrove Lane"
--                 ]
-- :}
--
-- >>> :{
--      putStrLn
--          $ display
--              $ mergeWithStrategy 
--                    (matchedStrategy (\_ store address -> MkStoreSummary (storeName store) address))
--                    stores
--                    addresses
-- :}
-- storeSummaryName | storeSummaryAddress
-- ---------------- | -------------------
--           "Maxi" |    1982 14th Avenue
--          "Metro" |      10 Main Street
--         "Sobeys" |    914 Prima Street
mergeWithStrategy :: ( Indexable t1, Indexable t2, Frameable t3
                     , Key t1 ~ Key t2
                     )
                  => MergeStrategy (Key t1) t1 t2 t3
                  -> Frame t1
                  -> Frame t2
                  -> Frame t3
mergeWithStrategy = mergeWithStrategyOn id id


-- | Merge two dataframes using a merging strategy, where the indexes
-- of the dataframes are mapped to some key of type @k@.
--
-- See `mergeWithStrategy` for further notes and examples.
mergeWithStrategyOn :: ( Ord k, Indexable t1, Indexable t2, Frameable t3)
                    => (Key t1 -> k) -- ^ How to map the index of the left dataframe onto a key of type @k@
                    -> (Key t2 -> k) -- ^ How to map the index of the right dataframe onto a key of type @k@
                    -> MergeStrategy k t1 t2 t3
                    -> Frame t1
                    -> Frame t2
                    -> Frame t3
mergeWithStrategyOn mapk1 mapk2 strat df1Unsorted df2Unsorted   
    = let df1 = sortRowsByKeyUniqueOn mapk1 df1Unsorted
          df2 = sortRowsByKeyUniqueOn mapk2 df2Unsorted
          ix1 = Data.Vector.map mapk1 $ index df1
          ix2 = Data.Vector.map mapk2 $ index df2
          -- Since df1 and df2 are sorted by key and their keys are unique, we 
          -- can safely use `Set.fromDistinctAscList`.
          fullIx = (Set.fromDistinctAscList $ Data.Vector.toList ix1) 
                                `Set.union` 
                   (Set.fromDistinctAscList $ Data.Vector.toList ix2)
          
          fullLeft  = reindex fullIx (Data.Vector.zip ix1 (toRows df1))
          fullRight = reindex fullIx (Data.Vector.zip ix2 (toRows df2))
       in fromRows $ Data.Vector.catMaybes 
                   $ Data.Vector.zipWith (\t1 t2 -> uncurry strat (asThese t1 t2))
                                         fullLeft
                                         fullRight
    
    where
        asThese :: Eq k => (k, Maybe a) -> (k, Maybe b) -> (k, These a b)
        asThese (k1, Just a) (k2, Nothing) = assert (k1==k2) (k1, This a)
        asThese (k1, Nothing) (k2, Just b) = assert (k1==k2) (k1, That b)
        asThese (k1, Just a) (k2, Just b)  = assert (k1==k2) (k1, These a b)
        -- The following line is unreachable since we know that the key `k`
        -- will be present in at least one of the two rows.
        asThese _ _ = error "impossible"
        
        reindex :: Ord k => Set.Set k -> Vector (k, Row t) -> Vector (k, Maybe (Row t))
        reindex fullix vs = Data.Vector.fromListN (Set.size fullix) 
                          $ Data.Foldable.toList 
                          $ go Empty 
                               (Seq.fromList $ Set.toAscList fullix) 
                               (Seq.fromList $ Data.Vector.toList vs)
            where
                -- We use `Seq` for the O(1) append
                -- Note that this function REQUIRES the rows to be sorted in
                -- ascending values of their key
                go :: Ord k 
                   => Seq (k, Maybe (Row t)) -- Accumulator
                   -> Seq k                  -- Full index
                   -> Seq (k, Row t)         -- Rows
                   -> Seq (k, Maybe (Row t))
                go acc Empty _ = acc
                go acc keys Empty = acc Seq.>< fmap (, Nothing) keys
                go acc (k:<|ks) queue@((rk, row):<|rs) = case k `compare` rk of
                    EQ -> go (acc Seq.|> (k, Just row)) ks rs
                    LT -> go (acc Seq.|> (k, Nothing)) ks queue
                    -- Since the full index includes all keys, it's not possible
                    -- the following case
                    GT -> error "impossible"


-- | A merge strategy is a function that describes how to
-- merge two rows together.
--
-- A merge strategy must handle three cases:
-- 
-- * Only the left row (v`This`);
-- * Only the right row (v`That`);
-- * Both the left and right rows (v`These`).
--
-- The simplest merge strategy is `matchedStrategy`. 
--
-- See examples in the documentation of `mergeWithStrategy`.
type MergeStrategy k t1 t2 t3
    = (k -> These (Row t1) (Row t2) -> Maybe (Row t3))


-- | Merge strategy which only works if both the left and right
-- rows are found.
--
-- If you are familiar with relational databases, `matchedStrategy`
-- is an inner join.
matchedStrategy :: (k -> Row t1 -> Row t2 -> Row t3)
                -> MergeStrategy k t1 t2 t3
matchedStrategy f k (These r1 r2) = Just $ f k r1 r2
matchedStrategy _ _ _ = Nothing


-- | Type family which allows for higher-kinded record types
-- in two forms:
--
-- * Single record type using t`Identity`, where @`Column` Identity a ~ a@ ;
-- * Record type whose elements are some other functor (usually `Vector`).
--
-- Types are created like regular record types, but each element
-- must have the type @`Column` f a@ instead of @a@. For example:
--
-- >>> :{
--      data Student f
--          = MkStudent { studentName      :: Column f String
--                      , studentAge       :: Column f Int
--                      , studentMathGrade :: Column f Char
--                      }
--          deriving (Generic, Frameable)
-- :}
type family Column (f :: Type -> Type) x where
    Column Identity x = x
    Column f x        = f x

-- | Type synonym for a record type with scalar elements
type Row (dt :: (Type -> Type) -> Type) = dt Identity

-- | Type synonym for a record type whose elements are arrays (columns)
type Frame (dt :: (Type -> Type) -> Type) = dt Vector


-- | Typeclass to generically derive the function `fromRows`.
class GFromRows tI tV where
    gfromRows :: Vector (tI a) -> (tV a)

instance GFromRows (Rec0 a) (Rec0 (Vector a)) where
    gfromRows = K1 . Data.Vector.map unK1
    {-# INLINEABLE gfromRows #-}

instance (GFromRows tI1 tV1, GFromRows tI2 tV2) 
    => GFromRows (tI1 :*: tI2) (tV1 :*: tV2) where
    gfromRows vs = let (xs, ys) = Data.Vector.unzip $ Data.Vector.map (\(x :*: y) -> (x, y)) vs
                    in gfromRows xs :*: gfromRows ys
    {-# INLINEABLE gfromRows #-}

instance GFromRows tI tV => GFromRows (M1 i c tI) (M1 i c tV) where
    gfromRows vs = M1 (gfromRows (Data.Vector.map unM1 vs))
    {-# INLINEABLE gfromRows #-}


-- | Typeclass to generically derive the function `toRows`.
class GToRows tI tV where
    gtoRows :: tV a -> Vector (tI a)

instance GToRows (Rec0 a) (Rec0 (Vector a)) where
    gtoRows = Data.Vector.map K1 . unK1
    {-# INLINEABLE gtoRows #-}

instance (GToRows tI1 tV1, GToRows tI2 tV2) 
    => GToRows (tI1 :*: tI2) (tV1 :*: tV2) where
    gtoRows (xs :*: ys) = Data.Vector.zipWith (:*:) (gtoRows xs) (gtoRows ys)
    {-# INLINEABLE gtoRows #-}

instance (GToRows tI tV) => GToRows (M1 i c tI) (M1 i c tV) where
    -- gtoRows :: M1 i c tV a -> Vector (M1 i c tI a)
    gtoRows = Data.Vector.map M1 . gtoRows . unM1
    {-# INLINEABLE gtoRows #-}

class GILookup tI tV where
    gilookup :: Int -> tV a -> Maybe (tI a)

instance GILookup (Rec0 a) (Rec0 (Vector a)) where
    gilookup ix vs = K1 <$> (unK1 vs) Data.Vector.!? ix

instance (GILookup tI1 tV1, GILookup tI2 tV2)
    => GILookup (tI1 :*: tI2) (tV1 :*: tV2) where
        gilookup ix (xs :*: ys) 
            = (:*:) 
                <$> (gilookup ix xs) 
                <*> (gilookup ix ys)

instance (GILookup tI tV) => GILookup (M1 i c tI) (M1 i c tV) where
    gilookup ix = fmap M1 . gilookup ix . unM1


class GFields r where
    gfields :: r a -> [(String, String)]

instance GFields r => GFields (M1 D x r) where
    gfields = gfields . unM1 

instance GFields t => GFields (M1 C x t) where
    gfields = gfields . unM1 

instance (Show r, Selector s) => GFields (M1 S s (Rec0 r)) where
    gfields (M1 (K1 r)) = [(selName (undefined :: M1 S s (Rec0 r) ()), show r)]

instance (GFields f, GFields g) => GFields (f :*: g) where
    gfields (x :*: y) = gfields x ++ gfields y

-- | Typeclass that endows any record type @t@ with the ability to be packaged
-- as a dataframe.
--
-- Under no circumstances should you write instances for `Frameable`; instead,
-- simply derive an instance of `Generic` for @t@. For example:
--
-- >>> :set -XDeriveAnyClass
-- >>> :{
--     data Store f
--          = MkStore { storeName    :: Column f String
--                    , storeId      :: Column f Int
--                    , storeAddress :: Column f String
--                    }
--          deriving (Generic, Frameable)
-- :}
class Frameable t where

    -- | Package single rows of type @t@ into a @`Frame` t@.
    pack :: Vector (Row t) -> Frame t
    
    default pack :: ( Generic (Row t)
                    , Generic (Frame t)
                    , GFromRows (Rep (Row t)) (Rep (Frame t))
                    ) 
                    => Vector (Row t) 
                    -> Frame t
    pack = to . gfromRows . Data.Vector.map from
    {-# INLINABLE pack #-}

    -- | Unpack a dataframe into rows
    unpack :: Frame t -> Vector (Row t)
    
    default unpack :: ( Generic (Row t)
                      , Generic (Frame t)
                      , GToRows (Rep (Row t)) (Rep (Frame t))
                      ) 
                     => Frame t 
                     -> Vector (Row t) 
    unpack = Data.Vector.map to . gtoRows . from
    {-# INLINABLE unpack #-}


    -- | Look up a row from the frame by integer index
    iindex :: Int -> Frame t -> Maybe (Row t)

    default iindex :: ( Generic (Frame t)
                      , Generic (Row t)
                      , GILookup (Rep (Row t)) (Rep (Frame t))
                      )
                    => Int
                    -> Frame t
                    -> Maybe (Row t)
    iindex ix = fmap to . gilookup ix . from

    -- | Return the field names associated with a row or frame.
    -- This is useful to display frames via `display`.
    fields :: Row t -> [(String, String)]
    
    default fields :: ( Generic (Row t)
                      , GFields (Rep (Row t))
                      )
                   => Row t
                   -> [(String, String)]
    fields = gfields . from


-- | Typeclass for dataframes with an index, a column or set of columns that can 
-- be used to search through rows.
--
-- An index need not be unique, but the type of its keys must be an instance of `Eq`.
class ( Frameable t
      , Eq (Key t) -- Effectively required for lookups
      , Ord (Key t) -- Effectively required for joins
      ) => Indexable t where

    -- | A type representing a lookup key for a dataframe.
    -- This can be a single field, or a compound key composed
    -- of multiple fields
    type Key t

    -- | How to create an index from a frame (@`Frame` t@). 
    -- This is generally done by using record selectors.
    index :: Frame t -> Vector (Key t)

{- NOTE: Indexable key and index

Ideally, the `Indexable` class provides two methods:

* key   :: Row t   -> Key t
* index :: Frame t -> Vector (Key t)

However, asking users to implement both methods is redundant and 
could lead to errors, since both methods must be coherent 
with each other. Consider the following example:

@
data Person f
    = MkPerson { firstName :: Column f String
               , lastName  :: Column f String
               }
    deriving (Generic, Frameable)

instance Indexable Person where
    type Key Person = String
    key = firstName
    index = lastName -- oops
@

We could instead use the `key` function to build the `index`, but this requires
converting a `Frame t` to rows, which is wasteful:

class Indexable t where
    type Key t

    key :: Row t -> Key t

    index :: Frame t -> Vector (Key t)
    index = Data.Vector.fromList . map key . toRows

Ideally, we would have a single method in the `Indexable` class:

@
class Indexable t where
    type Key t

    index :: t f -> Column f (Key t)
@

which would work for both f=t`Identity` and f=`Vector`. This actually works
for simple record selectors, e.g.:

@
instance Indexable Person where
    type Key Person = String
    index :: Person f -> Column f (Key Person)
    index = firstName
@

The problem arises with compound keys. How would you write this?

@
instance Indexable Person where
    type Key Person = (String, String)
    index :: Person f -> Column f (Key Person)
    -- Implementation for `Row t`:
    index row = (,) <$> firstName row <*> lastName row
    -- implementation for `Frame t`:
    index frame = Data.Vector.zipWith (,) (firstName frame) (lastName frame)
@

We can unify the signature of `index` in this case with:

@
    index x = compound (firstName x, lastName x)
        where
            compound :: ( Person f -> Column f a
                        , Person f -> Column f b
                        )
                     -> Person f
                     -> Column f (a, b)
@

We can create a typeclass to do this (and implement instances for f=t`Identity`
and f=`Vector`):

@
class Compound f where
    compound :: ( Person f -> Column f a
                , Person f -> Column f b
                )
                -> Person f
                -> Column f (a, b)

instance Compound Identity where
    compound (f, g) x = (f x, g x)

instance Compound Vector where
    compound (f, g) x = Data.Vector.zipWith (,) (f x) (g x)
@

Unfortunately, even with AllowAmbiguousTypes, I haven't been able to write 
an instance where type inference worked, e.g.:

@
instance Indexable Person where
    type Key Person = (String, String)

    index :: Compound f => Person f -> Column f (Key Person)
    index = compound (firstName, lastName)
@

-}


-- | Control how `displayWith` behaves.
data DisplayOptions t
    = DisplayOptions
    { maximumNumberOfRows  :: Int
    -- ^ Maximum number of rows shown. These rows will be distributed evenly
    -- between the start of the frame and the end
    , rowDisplayFunction :: Row t -> [(String, String)]
    -- ^ Function used to display rows from the frame. This should be a map from
    -- record name to value.
    }


-- | Default @`Frame` t@ display options.
defaultDisplayOptions :: Frameable t => DisplayOptions t
defaultDisplayOptions 
    = DisplayOptions { maximumNumberOfRows  = 6
                     , rowDisplayFunction = fields
                     }


-- | Display a @`Frame` t@ using default t'DisplayOptions'.
--
-- Although this is up to you, we strongly recommend that the `Show` 
-- instance for @`Frame` t@ be:
--
-- @
-- instance Show (Frame t) where show = display
-- @
--
-- Example:
-- 
-- >>> :{
--      data Student f
--          = MkStudent { studentName      :: Column f String
--                      , studentAge       :: Column f Int
--                      , studentMathGrade :: Column f Char
--                      }
--          deriving (Generic, Frameable)
-- :}
--
-- >>> students = fromRows $ Vector.fromList [MkStudent "Albert" 12 'C', MkStudent "Beatrice" 13 'B', MkStudent "Clara" 12 'A']
-- >>> putStrLn (display students)
-- studentName | studentAge | studentMathGrade
-- ----------- | ---------- | ----------------
--    "Albert" |         12 |              'C' 
--  "Beatrice" |         13 |              'B'
--     "Clara" |         12 |              'A'
display :: Frameable t
        => Frame t
        -> String
display = displayWith defaultDisplayOptions


-- | Display a @`Frame` t@ using custom t'DisplayOptions'.
--
-- Example:
-- 
-- >>> :{
--      data Student f
--          = MkStudent { studentName      :: Column f String
--                      , studentAge       :: Column f Int
--                      , studentMathGrade :: Column f Char
--                      }
--          deriving (Generic, Frameable)
-- :}
--
-- >>> :{
--     students = fromRows 
--              $ Vector.fromList 
--              [ MkStudent "Albert" 12 'C'
--              , MkStudent "Beatrice" 13 'B'
--              , MkStudent "Clara" 12 'A'
--              , MkStudent "David" 13 'A'
--              , MkStudent "Erika" 13 'D'
--              , MkStudent "Frank" 11 'C'
--              ]
-- :}
--
-- >>> putStrLn (displayWith (defaultDisplayOptions{maximumNumberOfRows=2}) students)
-- studentName | studentAge | studentMathGrade
-- ----------- | ---------- | ----------------
--    "Albert" |         12 |              'C' 
--         ... |        ... |              ...
--     "Frank" |         11 |              'C'
displayWith :: (Frameable t)
            => DisplayOptions t
            -> Frame t
            -> String
displayWith DisplayOptions{..} df 
    = if null df
        then "<Empty dataframe>" -- TODO: it IS possible to determine the record names
                                 --       without having any rows, but it requires
                                 --       an additional generic typeclass
        else formatGrid rows

    where
        len = length df
        n = max 1 (maximumNumberOfRows `div` 2)
        -- We prevent overlap between the 'head' rows and 'tail' rows
        -- by favoring removing duplicate integer indices from the tail rows
        headIxs = Set.fromList [0 .. n - 1]
        tailIxs = Set.fromList [len - n ..len] `Set.difference` headIxs
        headRows = catMaybes [ilookup i df | i <- Set.toList headIxs]
        tailRows = catMaybes [ilookup j df | j <- Set.toList tailIxs]

        firstRow = case headRows of
            [] -> error "Impossible!" -- We already checked that `df` won't be empty
            [xs] -> xs
            (xs:_) -> xs

        spacerRow = 
            if len > maximumNumberOfRows
                then [(map (second (const "...")) (fields firstRow))]
                else mempty
        rows = (fields <$> headRows) ++ spacerRow ++ (fields <$> tailRows)

        (headerLengths :: [(String, Int)]) = (map (\(k, _) -> (k, Prelude.length k)) (fields firstRow)) 
        (colWidths :: [(String, Int)]) 
            = map (second getMax) 
            $ List.foldl' 
                (\acc mp -> zipWith (\(k1, v1) (k2, v2) -> ((assert (k1 == k2) k1, v1 <> v2))) acc (map (second (Max . Prelude.length)) mp)) 
                (map (second Max) headerLengths) 
                rows

        -- | Format a grid represented by a list of rows, where every row is a list of items
        -- All columns will have a fixed width
        formatGrid :: [ [(String, String)]] -- List of rows
                   -> String
        formatGrid rs = mconcat $ List.intersperse "\n"
                                  $ [ mconcat $ List.intersperse " | " [ (pad w k) | (k, w) <- colWidths]]
                                 ++ [ mconcat $ List.intersperse " | " [ (pad w (replicate w '-')) | (_, w) <- colWidths]]
                                 ++ [ mconcat $ List.intersperse " | " [ (pad w v)
                                                                       | ((_, v), (_, w)) <- zip mp colWidths
                                                                       ]
                                    | mp <- rs
                                    ]
            where
                -- | Pad a string to a minimum of @n@ characters wide.
                pad :: Int -> String -> String 
                pad minNumChars s
                    | minNumChars <= Prelude.length s = s
                    | otherwise     = replicate (minNumChars - Prelude.length s) ' ' <> s