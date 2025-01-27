{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Frame
-- Copyright   :  (c) Laurent P. René de Cotret
-- License     :  MIT
-- Maintainer  :  laurent.decotret@outlook.com
-- Portability :  portable
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
    null, length, mapFrame, filterFrame, zipFramesWith, foldlFrame,
    -- * Displaying frames
    DisplayOptions(..), defaultDisplayOptions, display, displayWith,

    -- * Indexing operations
    -- ** Based on integer indices
    ilookup, iat,
    -- ** Based on indexable frames
    Indexable(Key, index), lookup, at
) where


import Control.Exception (assert)
import Data.Bifunctor (second)
import qualified Data.Foldable
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import qualified Data.List as List ( intersperse, foldl' )
import Data.Maybe (catMaybes)
import Data.Semigroup (Max(..))
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector
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
{-# INLINE[1] fromRows #-}

-- | Deconstruct a dataframe into its rows.
--
-- For the inverse operation, see `fromRows`.
toRows :: Frameable t 
       => Frame t
       -> Vector (Row t)
toRows = unpack
{-# INLINE[1] toRows #-}

-- TODO: Chaining operations such as `mapFrame` and `filterFrame`
--       should benefit from optimizing as `toRows . fromRows = id`
--       ( and `fromRows . toRows = id` as well).
--       See the rules below.
--       It's not clear if I'm using the rewrite system correctly,
--       by looking at the benchmark resuylts
{-# RULES
"fromRows/toRows" [2] forall xs. fromRows (toRows xs) = xs
"toRows/fromRows" [2] forall xs. toRows (fromRows xs) = xs 
  #-}

-- | Returns `True` if a dataframe has no rows.
null :: Frameable t
     => Frame t
     -> Bool
-- TODO: we can use yet another typeclass deriving
-- from generic to only look at ONE of the columns,
-- rather than reconstructing the first row
null = Data.Vector.null . toRows


length :: Frameable t
       => Frame t
       -> Int
-- TODO: we can use yet another typeclass deriving
-- from generic to only look at ONE of the columns,
-- rather than reconstructing all rows.
length = Data.Vector.length . toRows

-- | Map a function over each row individually.
mapFrame :: (Frameable t1, Frameable t2)
         => (Row t1 -> Row t2)
         -> Frame t1
         -> Frame t2
mapFrame f = fromRows 
          . Data.Vector.map f 
          . toRows


-- | Filter rows from a @`Frame` t@, only keeping
-- the rows where the predicate is `True`.
filterFrame :: (Frameable t)
            => (Row t -> Bool)
            -> Frame t
            -> Frame t
filterFrame f = fromRows 
             . Data.Vector.filter f
             . toRows


-- | Zip two frames together using a combination function.
-- Rows from each frame are matched in order; the resulting
-- frame will only contain as many rows as the shortest of
-- the two input frames
zipFramesWith :: (Frameable t1, Frameable t2, Frameable t3)
              => (Row t1 -> Row t2 -> Row t3)
              -> Frame t1
              -> Frame t2
              -> Frame t3
zipFramesWith f xs ys 
    = fromRows 
    $ Data.Vector.zipWith f 
                          (toRows xs)
                          (toRows ys)


-- | Left-associative fold of a structure but with strict application of the operator.
foldlFrame :: Frameable t
           => (b -> Row t -> b) -- ^ Reduction function that takes in individual rows
           -> b                 -- ^ Initial value for the accumulator
           -> Frame t           -- ^ Data frame
           -> b
foldlFrame f start 
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


-- | Type family which allows for higher-kinded record types
-- in two forms:
--
-- * Single record type using `Identity`, where @`Column` Identity a ~ a@ ;
-- * Record type whose elements are some other functor (usually `Vector`).
--
-- Types are created like regular record types, but each element
-- must have the type @`Column` f a@ instead of @a@.
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
-- simply derive an instance of `Generic` for @t@.
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
    -- This is useful to display frames
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
      ) => Indexable t where

    -- | A type representing a lookup key for a dataframe.
    -- This can be a single field, or a compound key composed
    -- of multiple fields
    type Key t

    -- | How to create an index from a @`Frame` t@. This is generally
    -- done by using record selectors.
    index :: Frame t -> Vector (Key t)


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


-- | Default 'Series' display options.
defaultDisplayOptions :: Frameable t => DisplayOptions t
defaultDisplayOptions 
    = DisplayOptions { maximumNumberOfRows  = 6
                     , rowDisplayFunction = fields
                     }


-- | Display a @`Frame` t@ using default 'DisplayOptions'.
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


-- | Display a @`Frame` t@ using custom 'DisplayOptions'.
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
                                 --       without having any rows
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