{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Frame
-- Copyright   :  (c) Laurent P. René de Cotret
-- License     :  MIT
-- Maintainer  :  laurent.decotret@outlook.com
-- Portability :  portable
--
-- This is an experimental interface to frames.
--
-- This module defines the type machinery and some functions to
-- process data frames. Data frames are structures where every
-- row corresponds to an object, but data is stored in
-- contiguous arrays known as columns.
--
-- To define a data frame, first create a record type using @data@:
--
--     data User f = 
--          MkUser { userName :: Column f String
--                 , userAge  :: Column f Int
--                 }
--          deriving (Generic)
--
-- There are two special things with this type definition:
--    * @User@ is a higher-kinded type, and admits a type constructor @f@. This
--      type constructor @f@ is used to distinguish between single rows and data frames, as we
--      will see in a second
--    * The type @User@ has an instance of `Generic` automatically derived.
--
-- In practice, the @f@ in @User f@ can only have two values: `Identity` and `Vector`.
-- @User `Identity`@ corresponds to a single row of a dataframe, while @User `Vector`@
-- corresponds to a data frame where each column (@userName@ and @userAge@) are really
-- arrays of values.
--
-- To make it more obvious, the type synonyms `Row` and `Frame` are provided, where 
-- @`Row` User@ is equivalent to:
--
--     data User =
--         MkUser { userName :: String
--                , userAge  :: Int
--                }
--
-- and `Frame User` is equivalent to:
-- 
--     data User =
--         MkUser { userName :: Vector String
--                , userAge  :: Vector Int
--                }
--
-- In order to unlock dataframe functionality, we need to derive an instance of `Frameable`
-- for @User@. This can be done automatically using:
--
--     instance Frameable User
--
-- The instance will be automatically generated through the @`Generic` User@ instance.
--
-- One small annoyance we must put up with is that deriving instances of `Show`, `Eq`, etc. 
-- for @User@ is now a little different:
--
--     deriving instance Show (Row User)
--     deriving instance Eq (Row User)
--
-- Finally, we are ready to do some data processing. First, we must build a dataframe
-- using `fromRows`:
--
-- >>> import Data.Vector as Vector
-- >>> users = fromRows $ Vector.fromList [MkUser "Albert" 12, MkUser "Beatrice" 35, MkUser "Clara" 24]
-- 
-- TODO: complete the tutorial
module Data.Frame (
    Column, Frameable, Row, Frame,
    -- * Basic interface
    fromRows, toRows, mapFrame, filterFrame, zipFramesWith, foldlFrame,

    -- * Indexing operations
    -- ** Based on integer indices
    ilookup, iat,
    -- ** Based on indexable frames
    Indexable(Key, index), lookup, at
) where

import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Vector (Vector)
import qualified Data.Vector
import GHC.Generics ( Generic(..), K1(..), Rec0, M1(..), type (:*:)(..) )
import Prelude hiding (lookup)


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

instance (GFromRows tI1 tV1, GFromRows tI2 tV2) 
    => GFromRows (tI1 :*: tI2) (tV1 :*: tV2) where
    gfromRows vs = let (xs, ys) = Data.Vector.unzip $ Data.Vector.map (\(x :*: y) -> (x, y)) vs
                    in gfromRows xs :*: gfromRows ys

instance GFromRows tI tV => GFromRows (M1 i c tI) (M1 i c tV) where
    gfromRows vs = M1 (gfromRows (Data.Vector.map unM1 vs))


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


-- | Typeclass that endows any record type @t@ with the ability to be packaged
-- as a dataframe.
--
-- Under no circumstances should you write instances for `Frameable`; instead,
-- simply derive an instance of `Generic` for @t@.
class Frameable t where

    -- | Package single rows of type @t@ into a @`Frame` t@.
    --
    -- To convert a @`Frame` t@ to rows, see `toRows`
    fromRows :: Vector (Row t) -> Frame t
    
    default fromRows :: ( Generic (Row t)
                        , Generic (Frame t)
                        , GFromRows (Rep (Row t)) (Rep (Frame t))
                        ) 
                     => Vector (Row t) 
                     -> Frame t
    fromRows = to . gfromRows . Data.Vector.map from

    -- | Package single rows of type @t@ into a @`Frame` t@.
    toRows :: Frame t -> Vector (Row t)
    
    default toRows :: ( Generic (t Identity)
                      , Generic (t Vector)
                      , GToRows (Rep (Row t)) (Rep (Frame t))
                      ) 
                     => Frame t 
                     -> Vector (Row t) 
    toRows = Data.Vector.map to . gtoRows . from


    -- | Look up a row from the frame by integer index
    --
    -- If you need to look up a particular row index and column, 
    -- `iat` is much faster.
    ilookup :: Int -> Frame t -> Maybe (Row t)

    default ilookup :: ( Generic (t Identity)
                       , Generic (t Vector)
                       , GILookup (Rep (Row t)) (Rep (Frame t))
                       )
                    => Int
                    -> Frame t
                    -> Maybe (Row t)
    ilookup ix = fmap to . gilookup ix . from



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


-- | Look up a row in a data frame by key.
--
-- If you need to look up a particular row and column, 
-- `at` is much faster.
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
iat :: Frame t 
    -> (Int, Frame t -> Vector a)
    -> Maybe a
fr `iat` (rowIx, col) = (col fr) Data.Vector.!? rowIx