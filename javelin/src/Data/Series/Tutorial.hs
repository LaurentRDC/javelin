{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-| This tutorial will get you started using 'Data.Series' -}

module Data.Series.Tutorial (
    -- * Introduction
    -- $introduction

    -- * Construction
    -- $construction

    -- * Index
    -- $index

    -- * Selections
    -- ** Single-key selection
    -- $singlekey

    -- ** Bulk selections
    -- $multikey

    -- * Filtering and mapping
    -- $filteringandmapping

    -- * Grouping
    -- $grouping

    -- * Window aggregation
    -- $windowing

    -- * Combining 'Series' together
    -- $zipping
    
    -- * Advanced topics
    -- ** Handling duplicate keys
    -- $duplicates

    -- ** Unboxed and generic series
    -- $unboxed

    -- ** Replacing values
    -- $replacement

    -- ** Comparison with other data structures
    -- $comparison

) where

import           Data.Series     ( Series, Occurrence, at, iat, select, to, from, upto, require
                                 , groupBy, aggregateWith, (<-|), (|->), Range, windowing
                                 )
import qualified Data.Series     as Series
import qualified Data.Series.Generic as Series (mean, std)
import qualified Data.Series.Generic
import           Data.Series.Index ( Index )
import qualified Data.Series.Index as Index
import qualified Data.Series.Unboxed
import           Data.Set        ( Set )
import qualified Data.Set
import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict
import qualified Data.Map.Merge.Strict
import           Numeric.Natural ( Natural)
import qualified Data.List
import qualified Data.Vector
import qualified Data.Vector.Unboxed

{- $introduction

    This is a short user guide on how to get started using @javelin@. 

-}

{- $construction 

The easiest way to create a 'Series' is to do it from a list using 'Data.Series.fromList':

>>> Series.fromList [ ('a', 1::Int), ('b', 2), ('c', 3), ('d', 4) ]
index | values
----- | ------
  'a' |      1
  'b' |      2
  'c' |      3
  'd' |      4

Note what happens when we have the same key (@\'a\'@) attached to multiple values:

>>> Series.fromList [ ('a', 1::Int), ('a', 0), ('b', 2), ('c', 3), ('d', 4) ]
index | values
----- | ------
  'a' |      0
  'b' |      2
  'c' |      3
  'd' |      4

'Series', like 'Map's, have unique keys; therefore, the output series may 
not be the same length as the input series. See further below for an 
explanation of how to handle duplicate keys. 

Since 'Series' are like 'Map', it's easy to convert between the two:

>>> let mp = Data.Map.Strict.fromList [ ('a', 0::Int), ('a', 1), ('b', 2), ('c', 3), ('d', 4) ]
>>> mp
fromList [('a',1),('b',2),('c',3),('d',4)]
>>> Series.fromStrictMap mp
index | values
----- | ------
  'a' |      1
  'b' |      2
  'c' |      3
  'd' |      4

Of course, 'Series.fromLazyMap' is also available.

-}

{- $index

'Series' have two components: values and an index.

The index (of type @'Index' k@) is an ordered set of unique elements which allows to determine 
where are each values in the series. Since all keys in an 'Index' are unique and sorted, it
is fast to find the value associated to any random key.

As we'll see soon, 'Index' is an important data structure which can be used to slice through a 'Series', 
so let's get comfortable with them.

>>> import qualified Data.Series.Index as Index

An 'Index' can be constructed from a list:

>>> Index.fromList [5::Int,5,4,3,2,1,5,5,5]
Index [1,2,3,4,5]

As you see above, repeated elements (in this case, @5@) won't be repeated in the 'Index'. Therefore, it often makes 
more sense to construct an 'Index' using 'Index.fromSet' from a 'Set' from "Data.Set".

One common way to construct an 'Index' is to programmatically __unfold__ a seed value using 
'Index.unfoldr'. Below, we want to generate numbers from 7 down to 1:

>>> Index.unfoldr (\x -> if x < 1 then Nothing else Just (x, x-1)) (7 :: Int)
Index [1,2,3,4,5,6,7]

This task is so common that there is a convenience function to create ranges, 'Index.range'. 
For example, if you want to create an 'Index' of values starting at 1 and ending at 10, in 
steps of 3:

>>> Index.range (+3) (1 :: Int) 10
Index [1,4,7,10]

An 'Index' is very much like a 'Set', so you can 

* check for membership using 'Index.member';
* combine two 'Index' using 'Index.union', 'Index.intersection', and 'Index.difference';
* find the integer index of a key using 'Index.lookupIndex';

and more.

-}

{- $singlekey 

Single-element selections are performed using 'at', which selects a single element by key. 'at' is safe;
if the key is missing, 'Nothing' is returned:

>>> let xs = Series.fromList [ ('a', 1::Int), ('b', 2), ('c', 3), ('d', 4) ]
>>> xs
index | values
----- | ------
  'a' |      1
  'b' |      2
  'c' |      3
  'd' |      4
>>> xs `at` 'a'
Just 1
>>> xs `at` 'z'
Nothing

-}

{- $multikey 

Bulk selection, also known as *slicing*, is the method by which we extract a sub-series from a series.
In the examples below, we'll assume that we have the series @aapl_close@ is available in-scope, which represents
the closing price of Apple stock:

>>> :{
let aapl_close = Series.fromList [ ("2010-01-04", 6.5522 :: Double)
                                 , ("2010-01-05", 6.5636)
                                 , ("2010-01-06", 6.4592)
                                 , ("2010-01-07", 6.4472)
                                 , ("2010-01-08", 6.4901)
                                 -- No prices during the weekend
                                 , ("2010-01-11", 6.5152)
                                 , ("2010-01-12", 6.4047)
                                 , ("2010-01-13", 6.3642)
                                 , ("2010-01-14", 6.4328)
                                 , ("2010-01-15", 6.4579)
                                 ]
    :}

Bulk selection is done via the 'select' function. 'select' works with many types of inputs. 
For example, we can query for a contiguous range of keys by using 'to':

>>> aapl_close `select` "2010-01-04" `to` "2010-01-08"
       index | values
       ----- | ------
"2010-01-04" | 6.5522
"2010-01-05" | 6.5636
"2010-01-06" | 6.4592
"2010-01-07" | 6.4472
"2010-01-08" | 6.4901

You can also request unbounded ranges. For example all dates up to @"2010-01-08"@ using 'upto':

>>> aapl_close `select` upto "2010-01-08"
       index | values
       ----- | ------
"2010-01-04" | 6.5522
"2010-01-05" | 6.5636
"2010-01-06" | 6.4592
"2010-01-07" | 6.4472
"2010-01-08" | 6.4901

There's also the other unbound range, 'from':

>>> aapl_close `select` from "2010-01-11"
       index | values
       ----- | ------
"2010-01-11" | 6.5152
"2010-01-12" | 6.4047
"2010-01-13" | 6.3642
"2010-01-14" | 6.4328
"2010-01-15" | 6.4579

Note that the bounds may contain less data than you think! For example, 
let's look at a 5-day range:

>>> aapl_close `select` "2010-01-08" `to` "2010-01-12"
       index | values
       ----- | ------
"2010-01-08" | 6.4901
"2010-01-11" | 6.5152
"2010-01-12" | 6.4047

We've requested a range of 5 days (@"2010-01-08"@, @"2010-01-09"@, @"2010-01-10"@, @"2010-01-11"@, @"2010-01-12"@), 
but there's no data in our series with the keys @"2010-01-09"@ and @"2010-01-10"@, because it was the week-end 
(stock markets are usually closed on week-ends). 

Sometimes you want to be more specific than a contiguous range of data; 'select' 
also supports bulk *random* access like so:

>>> aapl_close `select` ["2010-01-08", "2010-01-10", "2010-01-12"]
       index | values
       ----- | ------
"2010-01-08" | 6.4901
"2010-01-12" | 6.4047

Note above that we've requested data for the date @"2010-01-10"@, but it's missing. Therefore, 
the data isn't returned. If you want to get a sub-series which has the exact index that 
you've asked for, you can use 'require' in combination with an 'Index':

>>> import qualified Data.Series.Index as Index
>>> aapl_close `require` Index.fromList ["2010-01-08", "2010-01-10", "2010-01-12"]
       index |      values
       ----- |      ------
"2010-01-08" | Just 6.4901
"2010-01-10" |     Nothing
"2010-01-12" | Just 6.4047

Using 'require' or 'select' in conjunction with 'Index.range' is very powerful.

-}

{- $filteringandmapping 

'Series' support operations on both their index and their values. To illustrate 
this, let's load some latitude and longitude data for some cities.

We'll assume that the following types are in scope:

>>> import Data.Fixed (Centi)
>>> data Position = Pos { latitude :: Centi, longitude :: Centi } deriving (Show)
>>> :{
    let cities = Series.fromList [ ("Paris"::String , Pos  48.86    2.35)
                                 , ("New York City" , Pos  40.71   (-74.01))
                                 , ("Taipei"        , Pos  25.04    121.56)
                                 , ("Buenos Aires"  , Pos (-34.60) (-58.38)) 
                                 ]
    :}

We can easily filter for data just like you would filter a list. 
In this example, let's find cities in the western hemisphere (i.e. cities 
which have negative longitudes), using 'Series.filter':

>>> Series.filter (\pos -> longitude pos < 0) cities
          index |                                      values
          ----- |                                      ------
 "Buenos Aires" | Pos {latitude = -34.60, longitude = -58.38}
"New York City" |  Pos {latitude = 40.71, longitude = -74.01}

We can transform the values of a 'Series' using 'Series.map'. In this example, 
let's isolate the latitude of cities in the western hemisphere:

>>> let western_cities = Series.filter (\pos -> longitude pos < 0) cities
>>> Series.map latitude western_cities
          index | values
          ----- | ------
 "Buenos Aires" | -34.60
"New York City" |  40.71

Finally, we can summarize the 'Series' by reducing all its values. 
Let's average the latitude of cities in the western hemisphere:

>>> import Data.Series.Generic ( mean )
>>> let latitudes = Series.map latitude western_cities
>>> mean latitudes :: Double
3.055
-}

{- $grouping

One important feature of 'Series' is the ability to efficiently group values 
together based on their keys.

Let's load some stock price data for this part:

>>> import Data.Fixed ( Centi )
>>> (aapl_closing :: Series String Double) <- (Series.fromList . read) <$> readFile "files/aapl.txt"
>>> aapl_closing 
       index |  values
       ----- |  ------
"1980-12-12" |  0.1007
"1980-12-15" | 9.54e-2
"1980-12-16" | 8.84e-2
         ... |     ...
"2022-01-05" |  174.92
"2022-01-06" |   172.0
"2022-01-07" |  172.17

Note that normally we would use an appropriate datetime type for the index of @aapl_closing@, 
for example from the @time@ package <https://hackage.haskell.org/package/time>, but we're keeping 
it simple for this tutorial. 

Grouping involves two steps:

  (1) Grouping keys in some way using 'groupBy';
  (2) Aggregating the values in each group using 'aggregateWith' or other variants.

Let's find the highest closing price of each month. First, we need to define
our grouping function:

>>> :{ 
       -- | Extract the year and month from a date like XXXX-YY-ZZ. For example:
       -- 
       -- >>> month "2023-01-01"
       -- "2023-01"
       month :: String -> String
       month = take 7
    :}

Then, we can group keys by month and take the 'maximum' of each group:

>>> aapl_closing `groupBy` month `aggregateWith` maximum
    index | values
    ----- | ------
"1980-12" | 0.1261
"1981-01" | 0.1208
"1981-02" | 0.1007
      ... |    ...
"2021-11" |  165.3
"2021-12" | 180.33
"2022-01" | 182.01

This means, for example, that the maximum closing price for Apple stock in the 
month of November 2021 was $165.30 per share. This library also contains 
numerical aggregation functions such as 'Data.Series.mean' and 'Data.Series.std'. Therefore, in order 
to find the monthly average Apple closing price, rounded to the nearest cent:

>>> import Data.Series (mean)
>>> let (roundToCent :: Double -> Double) = \x -> fromIntegral ((round $ x * 100) :: Int) / 100
>>> aapl_closing `groupBy` month `aggregateWith` (roundToCent . mean)
    index | values
    ----- | ------
"1980-12" |   0.11
"1981-01" |   0.11
"1981-02" | 9.0e-2
      ... |    ...
"2021-11" | 154.21
"2021-12" | 173.55
"2022-01" | 176.16

-}

{- $windowing

Windowing aggregation refers to the practice of aggregating values in a window around every key.

General-purpose windowing is done using the 'windowing' function. Let's look at its
type signature:

>>> :t windowing
windowing
  :: Ord k =>
     (k -> Range k) -> (Series k a -> b) -> Series k a -> Series k b

Here, @`windowing` window aggfunc xs@ is a new series @'Series' k b@ where
for every key @k@, the values in the range @window k@ are aggregated by @aggfunc@
and placed in the resulting series at key @k@. Here's an example where
for every key @k@, we add the values at @k@ and @k+1@:

>>> :{ 
let (xs :: Series Int Int) 
      = Series.fromList [ (1, 0)
                        , (2, 1)
                        , (3, 2)
                        , (4, 3)
                        , (5, 4)
                        , (6, 5)
                        ]
in windowing (\k -> k `to` (k + 1)) sum xs
:}
index | values
----- | ------
    1 |      1
    2 |      3
    3 |      5
    4 |      7
    5 |      9
    6 |      5

'windowing' can be used to compute so-called rolling aggregations. An example of
this is to compute the rolling mean of the last 3 keys:

>>> import Data.Series ( mean )
>>> :{ 
let rollingMean = windowing (\k -> (k-3) `to` k) mean
    (xs :: Series Int Int) 
      = Series.fromList [ (1, 0)
                        , (2, 1)
                        , (3, 2)
                        , (4, 3)
                        , (5, 4)
                        , (6, 5)
                        ]
 in (rollingMean xs) :: Series Int Double
:}
index | values
----- | ------
    1 |    0.0
    2 |    0.5
    3 |    1.0
    4 |    1.5
    5 |    2.5
    6 |    3.5

-}

{- $zipping 

An important class of operations are combining two 'Series' together, also known as *zipping*. 
For lists, Haskell has 'Data.List.zipWith'. 'Series' also have 'Series.zipWith' and variants:

* 'Series.zipWith', which combines two series with some elementwise function;
* 'Series.zipWithMatched', which combines two series with some elementwise function 
  on keys which are in *both* maps;
* 'Series.zipWithStrategy', which combines two series with some elementwise 
  function and supports custom operations to deal with missing keys;

To illustrate the differences between the various zipping functions, 
consider the following two series. There's population:

>>> :set -XNumericUnderscores
>>> import Data.Fixed (Centi)
>>> :{ 
    -- Most recent population estimate rounded to the nearest million
    let population = Series.fromList [ ("Canada"::String, 40_000_000::Centi)
                                     , ("Kenya"         , 56_000_000)
                                     , ("Poland"        , 38_000_000)
                                     , ("Singapore"     ,  6_000_000)
                                     ]
    :}

and there's total land mass:

>>> :{ 
    -- Land mass in square kilometer
    let landmass = Series.fromList [ ("Brazil"::String, 8_520_000::Centi)
                                   , ("Canada",         9_990_000)
                                   , ("Kenya",            580_000)
                                   , ("Poland",           313_000)
                                   ] 
    :}

@'Series.zipWith' f left right@ combines the series @left@ and @right@ using the 
function @f@ which admits two arguments, for all keys one-by-one. If a key 
is missing from either @left@ or @right@, 'Series.zipWith' returns 'Nothing'. For example, 
the population density per country would be:

>>> Series.zipWith (/) population landmass
      index |      values
      ----- |      ------
   "Brazil" |     Nothing
   "Canada" |   Just 4.00
    "Kenya" |  Just 96.55
   "Poland" | Just 121.40
"Singapore" |     Nothing

Since we don't have population estimates for Brazil and no land mass 
information for Singapore, we can't calculate their population densities.

Sometimes, we only care about the results of @'Series.zipWith' f@ where keys are 
in both series. In this case, we can use 'Series.zipWithMatched':

>>> Series.zipWithMatched (/) population landmass
   index | values
   ----- | ------
"Canada" |   4.00
 "Kenya" |  96.55
"Poland" | 121.40

Finally, in case we want full control over what to do when a key is missing, 
we can use @Series.zipWithStrategy'. For example, consider the case where:

* If population numbers are missing, I want to set the density to 0;
* If land mass information is missing, I wait to skip calculating the density of this country. 

>>> import Data.Series (skipStrategy, constStrategy)
>>> let noPopulationStrategy = Series.constStrategy 0
>>> let noLandmassStrategy   = Series.skipStrategy
>>> Series.zipWithStrategy (/) noPopulationStrategy noLandmassStrategy population landmass
      index | values
      ----- | ------
   "Canada" |   4.00
    "Kenya" |  96.55
   "Poland" | 121.40
"Singapore" |   0.00

As you can imagine, 'Series.zipWithStrategy' is the most general and gives the most control, but is less easy 
to use than 'Series.zipWith' and 'Series.zipWithMatched'.

-}

{- $duplicates

If you must build a 'Series' with duplicate keys, you can use the 'Data.Series.fromListDuplicates' or 
'Data.Series.fromVectorDuplicates' functions. 
In the example below, the key @\'d\'@ is repeated three times:

>>> Series.fromListDuplicates [('b', 0::Int), ('a', 5), ('d', 1), ('d', -4), ('d', 7) ]
  index | values
  ----- | ------
('a',0) |      5
('b',0) |      0
('d',0) |      1
('d',1) |     -4
('d',2) |      7

Note that the 'Series' produced by 'Data.Series.fromListDuplicates' still has unique keys, but each key is a 
composite of a character and an occurrence. This is reflected in the type:

>>> :t Series.fromListDuplicates [('b', 0::Int), ('a', 5), ('d', 1), ('d', -4), ('d', 7) ]
Series.fromListDuplicates [('b', 0::Int), ('a', 5), ('d', 1), ('d', -4), ('d', 7) ]
  :: Series (Char, Occurrence) Int

Here, 'Data.Series.Occurrence' is a non-negative number, and can be converted to 
other integer-like numbers using 'fromIntegral'. In practice, you should aim to aggregate your 'Series' to remove duplicate keys, for example
using 'Data.Series.groupBy' and grouping on the first element of the key ('fst'):

>>> let xs = Series.fromListDuplicates [('b', 0::Int), ('a', 5), ('d', 1), ('d', -4), ('d', 7) ]
>>> xs `groupBy` fst `aggregateWith` sum
index | values
----- | ------
  'a' |      5
  'b' |      0
  'd' |      4

-}

{- $unboxed 

The 'Data.Series.Series' defined in "Data.Series" are based on 'Data.Vector.Vector' from "Data.Vector". 
This implementation is nice because such 'Series' can hold _any_ Haskell type. However, because
Haskell types can be arbitrarily complex, numerical operations on 'Series' may not be as fast
as could be.

For simpler types such as 'Double' and 'Int', a different kind of series can be used to
speed up numerical calculations: 'Data.Series.Unboxed.Series' from the "Data.Series.Unboxed" module.
Such 'Data.Series.Unboxed.Series' are much more limited: they can only contain datatypes which are
instances of 'Data.Vector.Unboxed.Unbox'. 

This then brings the question: how can you write software which supports both ordinary 'Data.Series.Series'
__and__ unboxed 'Data.Series.Unboxed.Series'? The answer is to use functions from the "Data.Series.Generic".

For example, we could implement the dot product of two series as:

>>> import qualified Data.Series.Generic as G
>>> import Data.Vector.Generic ( Vector )
>>> :{
      dot :: (Ord k, Num a, Vector v a) => G.Series v k a -> G.Series v k a -> a
      dot v1 v2 = G.sum $ G.zipWithMatched (*) v1 v2
    :}

You can convert between the two types of series using the 'Data.Series.Generic.convert' function.

-}

{- $replacement 

'Series.map' allows to map every value of a series. How about replacing *some* 
values in a series? The function 'Data.Series.replace' (and its infix variant, '|->') replaces values in the right operand 
which have an analogue in the left operand:

>>> import Data.Series ( (|->) )
>>> let nan = (0/0) :: Double
>>> let right = Series.fromList [('a', 1), ('b', nan), ('c', 3), ('d', nan)]
>>> right
index | values
----- | ------
  'a' |    1.0
  'b' |    NaN
  'c' |    3.0
  'd' |    NaN
>>> let left = Series.fromList [('b', 0::Double), ('d', 0), ('e', 0)]
>>> left
index | values
----- | ------
  'b' |    0.0
  'd' |    0.0
  'e' |    0.0
>>> left |-> right
index | values
----- | ------
  'a' |    1.0
  'b' |    0.0
  'c' |    3.0
  'd' |    0.0

In the example above, the key @\'e\'@ is ignored since it was not in the @right@ 
series to begin with.

The flipped version, '<-|', is also available.

-}

{- $comparison 

Below is a table showing which operations on "Data.Series" have analogues for 
other data structures.

+---------------------------------+--------------------------+---------------------------------+-------------------+----------------------+
| Action                          | "Data.Series"            | "Data.Map.Strict"               | "Data.List"       | "Data.Vector"        |
+=================================+==========================+=================================+===================+======================+
| Mapping values                  | 'Data.Series.map'        | 'Data.Map.Strict.map'           | 'fmap'            | 'Data.Vector.map'    |
+---------------------------------+--------------------------+---------------------------------+-------------------+----------------------+
| Mapping index                   | 'Data.Series.mapIndex'   | 'Data.Map.Strict.mapKeys'       |                   |                      |
+---------------------------------+--------------------------+---------------------------------+-------------------+----------------------+
| Mapping values with key         | 'Data.Series.mapWithKey' | 'Data.Map.Strict.mapWithKey'    |                   |                      |
+---------------------------------+--------------------------+---------------------------------+-------------------+----------------------+
| Filtering values                | 'Data.Series.filter'     | 'Data.Map.Strict.filter'        | 'filter'          | 'Data.Vector.filter' |
+---------------------------------+--------------------------+---------------------------------+-------------------+----------------------+
| Filtering index                 | 'Data.Series.select'     | 'Data.Map.Strict.filterWithKey' |                   |                      |
+---------------------------------+--------------------------+---------------------------------+-------------------+----------------------+
| Indexing by key                 | 'Data.Series.at'         | 'Data.Map.Strict.lookup'        |                   |                      |
+---------------------------------+--------------------------+---------------------------------+-------------------+----------------------+
| Indexing by position            | 'Data.Series.iat'        |                                 | 'Data.List.!'     | 'Data.Vector.!'      |
+---------------------------------+--------------------------+---------------------------------+-------------------+----------------------+
| Combine two structures key-wise | 'Data.Series.zipWith'    | 'Data.Map.Merge.Strict.merge'   |                   |                      |
+---------------------------------+--------------------------+---------------------------------+-------------------+----------------------+
| Union                           | 'Data.Series.<>'         | 'Data.Map.Strict.union'         | 'Data.List.union' |                      |
+---------------------------------+--------------------------+---------------------------------+-------------------+----------------------+
| Group keys                      | 'Data.Series.groupBy'    |                                 |                   |                      |
+---------------------------------+--------------------------+---------------------------------+-------------------+----------------------+

-}