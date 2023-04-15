
# Data.Series User Guide

This is a short user guide on how to get started using `opal`. The following document shows examples running in a Haskell interpreter (e.g. GHCi)

This guide is assuming that you have `opal` and `opal-io` installed. Let's start by setting up our environment:

``` haskell
> :set -XOverloadedStrings
> import           Data.Series ( Series )
> import qualified Data.Series as Series
> import qualified Data.Series.IO as IO
```

## Constructing `Series`

The easiest way to create a `Series` is to do it from a list:

```haskell
> Series.fromList [ ('a', 1::Int), ('b', 2), ('c', 3), ('d', 4) ]
index | values
----- | ------
  'a' |      1
  'b' |      2
  'c' |      3
  'd' |      4
```

Note what happens when we have the same key attached to multiple values:

```haskell
> Series.fromList [ ('a', 1::Int), ('a', 0), ('b', 2), ('c', 3), ('d', 4) ]
index | values
----- | ------
  'a' |      0
  'b' |      2
  'c' |      3
  'd' |      4
```

`Series`, like `Map`s, have unique keys; therefore, the output series above is not the same length as the input series, since the `'a'` key is repeated. Since `Series` are like `Map`, it's easy to convert between the two:

```haskell
> import qualified Data.Map.Strict as Map -- from the 'containers' package
> let mp = Map.fromList [ ('a', 0::Int), ('a', 1), ('b', 2), ('c', 3), ('d', 4) ]
> mp
fromList [('a',1),('b',2),('c',3),('d',4)]
> Series.fromStrictMap mp
index | values
----- | ------
  'a' |      1
  'b' |      2
  'c' |      3
  'd' |      4
```

Most data you might be interested in will be stored in data files, for example comma-separated values (CSV) files:

```haskell
> (aapl_close :: Series String Double) <- either error id <$> IO.readCSVFromFile "docs/data/AAPL.csv" "date" "close"
> aapl_close
       index |  values
       ----- |  ------
"1980-12-12" |  0.1007
"1980-12-15" | 9.54e-2
"1980-12-16" | 8.84e-2
         ... |     ...
"2022-01-05" |  174.92
"2022-01-06" |   172.0
"2022-01-07" |  172.17
```

## Selection

There are two kinds of selections that can be performed on a `Series`.

First, there are single-element selections using `at`, which selects a single element by key:

```haskell
> import Data.Series ( at )
> aapl_close `at` "1990-03-01"
Just 0.2457
```

Note that we can't know in advance if there is any data associated with the date `"1990-03-01"`, and so the answer might be `Nothing`. For example:

```haskell
> aapl_close `at` "2030-01-01"
Nothing
```

The other kind of selection is a bulk selection using `select`. For example, we may select data in a range of dates using `to`:

```haskell
> import Data.Series (select, to)
> aapl_close `select` "2010-01-04" `to` "2010-01-08"
       index | values
       ----- | ------
"2010-01-04" | 6.5522
"2010-01-05" | 6.5636
"2010-01-06" | 6.4592
"2010-01-07" | 6.4472
"2010-01-08" | 6.4901
```

Note that the bounds may contain less data than you think! For example, let's look at a 5-day range:

```haskell
> aapl_close `select` "2010-01-08" `to` "2010-01-12"
       index | values
       ----- | ------
"2010-01-08" | 6.4901
"2010-01-11" | 6.4328
"2010-01-12" | 6.3597
```

We've requested a range of 5 days (2010-01-08, 2010-01-09, 2010-01-10, 2010-01-11, 2010-01-12), but there's no data in our series with the keys 2010-01-09 and 2010-01-10, because it was the week-end (stock markets are usually closed on week-ends). 

Sometimes you want to be more specific than a contiguous range of data; `select` also supports bulk random access like so:

```haskell
> aapl_close `select` ["2010-01-08", "2010-01-12", "2010-01-16", "2010-01-22"]
       index | values
       ----- | ------
"2010-01-08" | 6.4901
"2010-01-12" | 6.3597
"2010-01-22" | 6.0544
```

Note above that we've requested data for the date `"2010-01-16"`, but it's missing. Therefore, the data isn't returned. If you want to get a sub-series which has the exact index that you've asked for, you can use `require` in combination with an `Index`:

```haskell
> import qualified Data.Series.Index as Index
> import Data.Series (require)
> aapl_close `require` Index.fromList ["2010-01-08", "2010-01-12", "2010-01-16", "2010-01-22"]
       index |      values
       ----- |      ------
"2010-01-08" | Just 6.4901
"2010-01-12" | Just 6.3597
"2010-01-16" |     Nothing
"2010-01-22" | Just 6.0544
```

See further below on a description of the `Index` type.

## Operations

`Series` support operations on both their index and their values. To illustrate this, let's load some latitude and longitude data for some cities:

```haskell
> import Data.Fixed (Centi)
> data Position = Pos { latitude :: Centi, longitude :: Centi } deriving (Show)
> :{ 
    let cities = Series.fromList [ ("Paris"::String , Pos  48.86    2.35)
                                 , ("New York City" , Pos  40.71   (-74.01))
                                 , ("Taipei"        , Pos  25.04    121.56)
                                 , ("Buenos Aires"  , Pos (-34.60) (-58.38)) 
                                 ]
    :}
> cities
          index |                                      values
          ----- |                                      ------
 "Buenos Aires" | Pos {latitude = -34.60, longitude = -58.38}
"New York City" |  Pos {latitude = 40.71, longitude = -74.01}
        "Paris" |    Pos {latitude = 48.86, longitude = 2.35}
       "Taipei" |  Pos {latitude = 25.04, longitude = 121.56}
```

We can easily filter for data just like you would filter a list. In this example, let's find cities in the western hemisphere (i.e. cities which have negative longitudes), using `Series.filter`:

```haskell
> Series.filter (\pos -> longitude pos < 0) cities
          index |                                      values
          ----- |                                      ------
 "Buenos Aires" | Pos {latitude = -34.60, longitude = -58.38}
"New York City" |  Pos {latitude = 40.71, longitude = -74.01}
```

We can transform the values of a `Series` using `Series.map`. In this example, let's isolate the latitude of cities in the western hemisphere:

```haskell
> let western_cities = Series.filter (\pos -> longitude pos < 0) cities
> Series.map latitude western_cities
          index | values
          ----- | ------
 "Buenos Aires" | -34.60
"New York City" |  40.71
```

Finally, we can summarize the `Series` by reducing all its values. In this example, let's average the latitude of cities in the western hemisphere:

```haskell
> import Data.Series.Generic ( mean )
> let western_cities = Series.filter (\pos -> longitude pos < 0) cities
> let latitudes = Series.map latitude western_cities
> mean latitudes
3.055
```

## Combining `Series` together

An important class of operations are combining two series together. For lists and vectors, Haskell has `zipWith`. `Series` also have `zipWith` and  variants:
* `zipWith`, which combines two series with some elementwise function;
* `zipWithMatched`, which combines two series with some elementwise function on keys which are in *both* maps;
* `zipWithStrategy`, which combines two series with some elementwise function and supports custom operations to deal with missing keys;

To illustrate the differences between the various `zip*` functions, consider the following two series of contry populations and total land mass:

```haskell
> :set -XNumericUnderscores
> -- We'll be using `Centi` to limit results to 2 decimals
> import Data.Fixed (Centi)
> :{ 
    -- Most recent population estimate rounded to the nearest million
    let population = Series.fromList [ ("Canada"::String, 40_000_000::Centi)
                                     , ("Kenya"         , 56_000_000)
                                     , ("Poland"        , 38_000_000)
                                     , ("Singapore"     ,  6_000_000)
                                     ]
  :}
> :{ 
     -- Land mass in square kilometer
    let landmass = Series.fromList [ ("Brazil"::String, 8_520_000::Centi)
                                   , ("Canada",         9_990_000)
                                   , ("Kenya",            580_000)
                                   , ("Poland",           313_000)
                                   ] 
  :}
```

`zipWith f left right` combines the series `left` and `right` using the function `f` which admits two arguments, for all keys one-by-one. If a key is missing from either `left` or `right`, `zipWith` returns `Nothing`. For example, the population density per country would be:

```haskell
> Series.zipWith (/) population landmass
      index |      values
      ----- |      ------
   "Brazil" |     Nothing
   "Canada" |   Just 4.00
    "Kenya" |  Just 96.55
   "Poland" | Just 121.40
"Singapore" |     Nothing
```

Since we don't have population estimates for Brazil and no land mass information for Singapore, we can't calculate their population densities.

Sometimes, we only care about the results of `zipWith f` where keys are in both series. In this case, we can use `zipWithMatched`:

```haskell
> Series.zipWithMatched (/) population landmass
   index | values
   ----- | ------
"Canada" |   4.00
 "Kenya" |  96.55
"Poland" | 121.40
```

Finally, in case we want full control over what to do when a key is missing, we can use `zipWithStrategy`. For example, consider the case where:

* If population numbers are missing, I want to set the density to 0;
* If land mass information is missing, I wait to skip calculating the density of this country. 

```haskell
> import Data.Series (skipStrategy, constStrategy)
> :{ 
       Series.zipWithStrategy (/)
                              (constStrategy 0) -- What to do when the country is missing from `population`
                              skipStrategy      -- What to do when the country is missing from `landmass`
                              population
                              landmass
  :}
      index | values
      ----- | ------
   "Canada" |   4.00
    "Kenya" |  96.55
   "Poland" | 121.40
"Singapore" |   0.00
```

As you can imagine, `zipWithStrategy` is the most general and gives the most control, but is less easy to use than `zipWith` and `zipWithMatched`.

## Replacing certain values

`Series.map` allows to map every value of a series. How about replacing *some* values in a series? The function `(|->)` replaces values in the right operand which have an analogue in the left operand. The flipped version, `(<-|)`, is also available:

```haskell
> import Data.Series ( (|->), (<-|) )
> let nan = (0/0) :: Double
> let right = Series.fromList [('a', 1), ('b', nan), ('c', 3), ('d', nan)]
> right
index | values
----- | ------
  'a' |    1.0
  'b' |    NaN
  'c' |    3.0
  'd' |    NaN
> let left = Series.fromList [('b', 0::Double), ('d', 0), ('e', 0)]
> left
index | values
----- | ------
  'b' |    0.0
  'd' |    0.0
  'e' |    0.0
> left |-> right
index | values
----- | ------
  'a' |    1.0
  'b' |    0.0
  'c' |    3.0
  'd' |    0.0
> right <-| left
index | values
----- | ------
  'a' |    1.0
  'b' |    0.0
  'c' |    3.0
  'd' |    0.0
```

In the example above, the key `'e'` is ignored since it was not in the `right` series to begin with.

The flipped version, `(<-|)`, is also available.

## Grouping

One important feature of series is the ability to efficiently group values together based on their keys.

Let's load some stock price data for this part:

```haskell
> (aapl_close :: Series String Double) <- either error id <$> IO.readCSVFromFile "docs/data/AAPL.csv" "date" "close" 
> aapl_close
       index |  values
       ----- |  ------
"1980-12-12" |  0.1007
"1980-12-15" | 9.54e-2
"1980-12-16" | 8.84e-2
         ... |     ...
"2022-01-05" |  174.92
"2022-01-06" |   172.0
"2022-01-07" |  172.17
```

Note that normally we would use an appropriate datetime type for the index of `aapl_close`, for example from the [`time`](https://hackage.haskell.org/package/time) package, but we're keeping it simple for this user guide. 

We can find the highest closing price of each month by grouping each month using `groupBy`:

```haskell
> import Data.Series (groupBy, aggregateWith)
> :{ 
       -- | Extract the year and month from a date like XXXX-YY-ZZ. For example:
       -- 
       -- >>> month "2023-01-01"
       -- "2023-01"
       month :: String -> String
       month = take 7
  :}
> aapl_close `groupBy` month `aggregateWith` maximum
    index | values
    ----- | ------
"1980-12" | 0.1261
"1981-01" | 0.1208
"1981-02" | 0.1007
      ... |    ...
"2021-11" |  165.3
"2021-12" | 180.33
"2022-01" | 182.01
``` 

This means, for example, that that maximum closing price for AAPL stock in the month of November 2021 was $165.30 per share. This library also contains numerical aggregation functions such as `mean` and `std`. Therefore, in order to find the monthly average AAPL closing price, rounded :

```haskell
> import Data.Series (mean)
> roundToCent x = fromIntegral (round $ x * 100) / 100
> aapl_close `groupBy` month `aggregateWith` (roundToCent . mean)
    index | values
    ----- | ------
"1980-12" |   0.11
"1981-01" |   0.11
"1981-02" |   0.09
      ... |    ...
"2021-11" | 154.21
"2021-12" | 173.55
"2022-01" | 176.16
```

## Comparison with other data structures

Below is a table showing which operations on `Data.Series` have analogues for other data structures.

<!-- Table generated using: https://www.tablesgenerator.com/markdown_tables -->
| Action                          | `Data.Series`            | `Data.Map.Strict`               | `Data.List`       | `Data.Vector`        |
|---------------------------------|--------------------------|---------------------------------|-------------------|----------------------|
| Mapping values                  | `Data.Series.map`        | `Data.Map.Strict.map`           | `fmap`            | `Data.Vector.map`    |
| Mapping index                   | `Data.Series.mapIndex`   | `Data.Map.Strict.mapKeys`       | N/A               | N/A                  |
| Mapping values with key         | `Data.Series.mapWithKey` | `Data.Map.Strict.mapWithKey`    | N/A               | N/A                  |
| Filtering values                | `Data.Series.filter`     | `Data.Map.Strict.filter`        | `filter`          | `Data.Vector.filter` |
| Filtering index                 | `Data.Series.select`     | `Data.Map.Strict.filterWithKey` | N/A               | N/A                  |
| Indexing by key                 | `Data.Series.at`         | `Data.Map.Strict.lookup`        | N/A               | N/A                  |
| Indexing by position            | `Data.Series.iat`        | N/A                             | `Data.List.(!)`   | `Data.Vector.(!)`    |
| Combine two structures key-wise | `Data.Series.zipWith`    | `Data.Map.Merge.Strict.merge`   | N/A               | N/A                  |
| Union                           | `Data.Series.(<>)`       | `Data.Map.Strict.union`         | `Data.List.union` | N/A                  |
| Group keys                      | `Data.Series.groupBy`    | N/A                             | N/A               | N/A                  |