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
-- This module is considered __internal__. Using the 'Series' constructor
-- directly may result in loss or corruption of data if not handled carefully.
--
-- The Package Versioning Policy still applies.

module Data.Series.Generic.Internal ( 
    -- * Constructor
    Series(..),
    -- * Unsafe construction
    fromDistinctAscList,
    fromDistinctAscVector,
    -- * Unsafe selection
    selectSubset
) where

import Data.Series.Generic.Definition   ( Series(..), fromDistinctAscList, fromDistinctAscVector )
import Data.Series.Generic.View         ( selectSubset )