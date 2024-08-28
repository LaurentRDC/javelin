{-# LANGUAGE NoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  $header
-- Copyright   :  (c) Laurent P. René de Cotret
-- License     :  MIT
-- Maintainer  :  laurent.decotret@outlook.com
-- Portability :  portable
--
-- This module contains data structures and functions to work with any type of 'Series', 
-- including boxed and unboxed types.
--
-- Use the definitions in this module if you want to support all types of 'Series' at once.
module Data.Series.Generic (
    -- * Definition
    Series(index, values),
    convert,

    -- * Building/converting 'Series'
    singleton, fromIndex,
    -- ** Lists
    fromList, toList,
    -- ** Vectors
    fromVector, toVector,
    -- ** Handling duplicates
    Occurrence, fromListDuplicates, fromVectorDuplicates,
    -- ** Strict Maps
    fromStrictMap, toStrictMap,
    -- ** Lazy Maps
    fromLazyMap, toLazyMap,
    -- ** Ad-hoc conversion with other data structures
    IsSeries(..),

    -- * Mapping and filtering
    map, mapWithKey, mapIndex, concatMap, filter, filterWithKey, 
    take, takeWhile, drop, dropWhile,
    -- ** Mapping with effects
    mapWithKeyM, mapWithKeyM_, forWithKeyM, forWithKeyM_, traverseWithKey,

    -- * Folding
    fold, foldM, foldWithKey, foldMWithKey, foldMap, foldMapWithKey,
    -- ** Specialized folds
    mean, variance, std, 
    length, null, all, any, and, or, sum, product, maximum, maximumOn, minimum, minimumOn,
    argmax, argmin,

    -- * Scans
    postscanl, prescanl, forwardFill,

    -- * Combining series
    zipWith, zipWithMatched, zipWithKey,
    zipWith3, zipWithMatched3, zipWithKey3,
    ZipStrategy, skipStrategy, mapStrategy, constStrategy, zipWithStrategy, zipWithStrategy3,
    zipWithMonoid, esum, eproduct, unzip, unzip3,

    -- * Index manipulation
    require, requireWith, catMaybes, dropIndex,

    -- * Accessors
    -- ** Bulk access
    select, selectWhere, Range, to, from, upto, Selection, 
    -- ** Single-element access
    at, iat,

    -- * Replacement
    replace, (|->), (<-|),

    -- * Grouping and windowing operations
    groupBy, Grouping, aggregateWith, foldWith, 
    windowing, expanding,

    -- * Displaying 'Series'
    display, displayWith,
    noLongerThan,
    DisplayOptions(..), defaultDisplayOptions
) where

import Control.Foldl                    ( mean, variance, std )
import Data.Series.Generic.Aggregation  ( groupBy, Grouping, aggregateWith, foldWith
                                        , windowing, expanding, all, any, and, or, sum, product, maximum, maximumOn, minimum, minimumOn
                                        , argmax, argmin,
                                        )
import Data.Series.Generic.Definition   ( Series(index, values), IsSeries(..), Occurrence, convert, singleton, fromIndex, fromStrictMap
                                        , toStrictMap, fromLazyMap, toLazyMap, fromList, fromListDuplicates, toList
                                        , fromVector, fromVectorDuplicates, toVector
                                        , map, mapWithKey, mapIndex, concatMap, length, null, take, takeWhile, drop, dropWhile
                                        , mapWithKeyM, mapWithKeyM_, forWithKeyM, forWithKeyM_, traverseWithKey, fold, foldM
                                        , foldWithKey, foldMWithKey, foldMap, foldMapWithKey
                                        , display, displayWith, noLongerThan, DisplayOptions(..), defaultDisplayOptions
                                        )
import Data.Series.Generic.Scans        ( postscanl, prescanl, forwardFill )
import Data.Series.Generic.View         ( Range, Selection, at, iat, select, selectWhere, to, from, upto, filter, filterWithKey, require, requireWith
                                        , catMaybes, dropIndex,
                                        )
import Data.Series.Generic.Zip          ( zipWith, zipWithMatched, zipWithKey, zipWith3, zipWithMatched3, zipWithKey3, replace
                                        , (|->), (<-|), zipWithStrategy, zipWithStrategy3, ZipStrategy, skipStrategy, mapStrategy, constStrategy
                                        , zipWithMonoid, esum, eproduct, unzip, unzip3
                                        )
