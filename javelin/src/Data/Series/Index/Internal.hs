{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Series.Generic.Internal
-- Copyright   :  (c) Laurent P. René de Cotret
-- License     :  MIT
-- Maintainer  :  laurent.decotret@outlook.com
-- Portability :  portable
--
-- = WARNING
--
-- This module is considered __internal__. It contains functions
-- which may be unsafe to use in general, for example requiring 
-- the data to be pre-sorted like 'fromDistinctAscList'.
--
-- The Package Versioning Policy still applies.

module Data.Series.Index.Internal(
    Index(..),

    -- * Unsafe construction
    fromAscList,
    fromDistinctAscList,
    fromAscVector,
    fromDistinctAscVector,

    -- * Unsafe functions with pre-conditions
    mapMonotonic,

    -- * Unsafe indexing
    elemAt,
    findIndex,

) where

import Data.Series.Index.Definition ( Index(..), fromAscList, fromDistinctAscList, fromAscVector
                                    , fromDistinctAscVector, mapMonotonic, elemAt, findIndex
                                    )
