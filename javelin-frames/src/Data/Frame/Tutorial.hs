{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- |
-- Module      :  $header
-- Copyright   :  (c) Laurent P. René de Cotret
-- License     :  MIT
-- Maintainer  :  laurent.decotret@outlook.com
-- Portability :  portable
--
module Data.Frame.Tutorial (
    -- * Introduction
    -- $introduction

    -- * Quick tour
    -- $quicktour

) where

import Data.Frame
import Data.Vector as Vector
import GHC.Generics (Generic)

{- $introduction

-}

{- $quicktour
Let's look at a real example. First, let's get some setup out of the way. We must 
activate @-XDeriveAnyClass@ to automatically derive `Frameable`:

>>> :set -XDeriveAnyClass

and we'll import the "Data.Vector" module as well:

>>> import Data.Vector as Vector

We define

>>> :{
     data Student f
         = MkStudent { studentName      :: Column f String
                     , studentAge       :: Column f Int
                     , studentMathGrade :: Column f Char
                     }
         deriving (Generic, Frameable)
    :}

We use `fromRows` to pack individual students into a dataframe:

>>> :{
    students = fromRows 
             $ Vector.fromList [ MkStudent "Albert" 12 'C'
                               , MkStudent "Beatrice" 13 'B'
                               , MkStudent "Clara" 12 'A'
                               ]
    :}

We can render the dataframe @students@ into a nice string using `display` 
(and print that string using using `putStrLn`):

>>> putStrLn (display students)
studentName | studentAge | studentMathGrade
----------- | ---------- | ----------------
   "Albert" |         12 |              'C' 
 "Beatrice" |         13 |              'B'
    "Clara" |         12 |              'A'

== Operations on rows

Many operations that treat a dataframe as an array
of rows are provided.

There's `mapFrame` to map each row to a new structure:

>>> :{
    putStrLn 
        $ display 
            $ mapFrame 
                (\(MkStudent name age grade) -> MkStudent name (2*age) grade) 
                students
:}
studentName | studentAge | studentMathGrade
----------- | ---------- | ----------------
   "Albert" |         24 |              'C' 
 "Beatrice" |         26 |              'B'
    "Clara" |         24 |              'A'

There's `filterFrame` to keep specific rows:

>>> :{
    putStrLn 
        $ display 
            $ filterFrame 
                (\(MkStudent _ _ grade) -> grade < 'C') 
                students
:}
studentName | studentAge | studentMathGrade
----------- | ---------- | ----------------
 "Beatrice" |         13 |              'B'
    "Clara" |         12 |              'A'

Finally, there's `foldlFrame` to summarize a dataframe by using whole rows:

>>> import Data.Char (ord)
>>> :{
    foldlFrame 
        (\acc (MkStudent _ age grade) -> acc + age + ord grade) 
        (0 :: Int) 
        students
:}
235
-}

{- $construction 

Records

-}