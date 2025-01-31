{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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

    -- * Quick start
    -- $quickstart
    
    -- * Defining types
    -- $construction

    -- * Advanced indexing
    -- $advindexing

) where

import Data.Frame as Frame
import Data.Functor.Identity (Identity)
import Data.Vector as Vector
import GHC.Generics (Generic)

{- $introduction

This is a short user guide on how to get started using @javelin-frames@.

The central data structure at the heart of this package is the dataframe. 
A dataframe, represented by @`Frame` t@ for some record-type @t@, is a 
record whose values are arrays representing columns.

-}

{- $quickstart
Let's look at a real example. We'll import the "Data.Frame" module to disambiguate
some functions:

>>> import Data.Frame as Frame

and we need extensions to derive instances automatically:

>>> :set -XDeriveGeneric
>>> :set -XDeriveAnyClass

We define

>>> :{
    data Student f
         = MkStudent { studentName      :: Column f String
                     , studentAge       :: Column f Int
                     , studentMathGrade :: Column f Char
                     }
         deriving (Generic)
    deriving instance Frameable Student
    -- We need to derive other instances (Show, Eq, ...)
    -- separately
    deriving instance Show (Row Student)
:}

It is key to derive the instance of @`Frameable` Student@, which unlocks
almost all of the functionality of this package.

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

== Operations on columns

A dataframe is a columnar data structure; operations on columns are very efficient.

We can query for a column using a field selector, just like a normal record:

>>> studentName students
["Albert","Beatrice","Clara"]

Although the notation suggests that this is a list, columns are really `Vector`:

>>> :t (studentName students)
(studentName students) :: Vector [Char]

This means that you can use the efficient operations provided by the "Data.Vector"
module to operate on columns.

== Operations on rows

Many operations that treat a dataframe as an array
of rows are provided.

There's `mapRows` to map each row to a new structure:

>>> :{
    putStrLn 
        $ display 
            $ mapRows 
                (\(MkStudent name age grade) -> MkStudent name (2*age) grade) 
                students
:}
studentName | studentAge | studentMathGrade
----------- | ---------- | ----------------
   "Albert" |         24 |              'C' 
 "Beatrice" |         26 |              'B'
    "Clara" |         24 |              'A'

There's `filterRows` to keep specific rows:

>>> :{
    putStrLn 
        $ display 
            $ filterRows 
                (\(MkStudent _ _ grade) -> grade < 'C') 
                students
:}
studentName | studentAge | studentMathGrade
----------- | ---------- | ----------------
 "Beatrice" |         13 |              'B'
    "Clara" |         12 |              'A'

Finally, there's `foldlRows` to summarize a dataframe by using whole rows:

>>> import Data.Char (ord)
>>> :{
    foldlRows 
        (\acc (MkStudent _ age grade) -> acc + age + ord grade) 
        (0 :: Int) 
        students
:}
235

== Lookups

Since dataframes are highly structured, we can efficiently query
them in two flavours: querying by integer index, and querying by key.

=== Querying by integer index

Querying by integer index is supported for all dataframes. Use
the `ilookup` function to retrive a row:

>>> ilookup 0 students
Just (MkStudent {studentName = "Albert", studentAge = 12, studentMathGrade = 'C'})
>>> ilookup 2 students
Just (MkStudent {studentName = "Clara", studentAge = 12, studentMathGrade = 'A'})
>>> ilookup 1000 students
Nothing

If we need the specific field of a specific row, it is much more efficient
to use `iat`:

>>> students `iat` (1, studentMathGrade)
Just 'B'

=== Querying by key

Querying by integer index may not be natural, and could be error-prone.
We can specify a column (or set of columns) that represent our
dataframe index. Just like a database table, an index speeds up
the lookup of a dataframe by key.

To do this, we must write an instance of `Indexable` for our type @Student@,
where we want to be able to efficiently search by student name:

>>> :set -XTypeFamilies
>>> :{
    instance Indexable Student where
        type Key Student = String
        index = studentName
:}

Now, we can use the functions `Frame.lookup` and `at` (similar to `ilookup` 
and `iat`, respectively) which take key (in our case, student names) 
instead of integer indices.

>>> Frame.lookup "Beatrice" students
Just (MkStudent {studentName = "Beatrice", studentAge = 13, studentMathGrade = 'B'})

>>> Frame.lookup "Vivienne" students
Nothing

>>> students `at` ("Albert", studentAge)
Just 12

And there you have it! This was a quick tour. Read on to learn more details
and more advanced functionality.
-}

{- $construction 

To start using the machinery of this package, one must define the appropriate type.
Types that can be turned into dataframes are non-empty, higher-kinded record types.
In particular, every field must make use of the `Column` type family.

Let's look at an example:

>>> newtype Address = MkAddress String deriving (Show, Eq)
>>> data Merchandise = Clothes | Food | Cars deriving (Show, Eq)
>>> :{
    data Store f
        = MkStore { storeName        :: Column f String
                  , storeAddress     :: Column f Address
                  , storeId          :: Column f Int
                  , storeMerchandise :: Column f Merchandise
                  }
        deriving (Generic)
:}

Here, we define a higher-kinded record type @Store@ with four fields. 
The type parameter @f@ allows the various functions in this package
to switch between a column-oriented format and single-rows.

In practice the type @f@ can only be `Identity` (for a single row),
or `Vector` (for a dataframe)

For ergonomics, the type synonym @`Row` t@ is provided to represent a 
single row. The type synonym @`Frame` t@ is provided to represent
a dataframe.

One caveat of this design is that instances (e.g. for `Show` or `Eq`)
must be defined in separate expressions:

>>> deriving instance Show (Row Store)
>>> deriving instance Eq (Row Store)

Let's consider a single @Store@:

>>> (MkStore "Maxi" (MkAddress "17 Delicious Av.") 1 Food) :: Row Store
MkStore {storeName = "Maxi", storeAddress = MkAddress "17 Delicious Av.", storeId = 1, storeMerchandise = Food}

so @`Row` Store@ is exactly what we would expect from Haskell's regular
record types.

In order to access dataframe functionality, we need to ask our code
to generate some boilerplate automatically. We do this by deriving an 
instance of `Frameable`:

>>> :set -XDeriveAnyClass
>>> deriving instance Frameable Store

Note that deriving an instance of `Frameable` requires that our type @Store@ have
a `Generic` instance. This allows @javelin-frames@ to inspect our type @Store@
and write an implementation of `Frameable` automatically.

== Limitations

At this time, `Frameable` can only be derived for higher-kinded record types that
do NOT nest. For example, consider the following hierarchy:

>>> :{
    data Location f
        = MkLocation { longitude :: Column f Double
                     , latitude  :: Column f Double
                     , elevation :: Column f Double
                     }
        deriving (Generic, Frameable)
    data Company f
        = MkCompany { companyName    :: Column f String
                    , companyId      :: Column f Int
                    , companyAddress :: Location f -- Nesting happens here
                  }
        deriving (Generic)
:}

The following will unfortunately fail with a potentially confusing type error:

@
deriving instance Frameable Company
@

Are you an expert in generics who wants to help us figure it out? Feel free to 
[raise an issue or open a pull request](https://github.com/LaurentRDC/javelin).
-}

{- $advindexing

For some record type @t@ with an instance of `Frameable`, we can query for specific
rows and elements using `ilookup` and `iat` respectively.

However, many types can naturally be indexed by a subset of the columns, which becomes a key
This key is similar to primary keys in databases.

We can derive an instance of `Indexable` to allow us to query data from a 
dataframe not by the integer index of the rows, but by some key instead.

== Simple keys

The simplest example is that of keys derived from a single column. 

We start with a data definition:

>>> :{
    newtype Address = Addr String deriving (Show)
    data Store f
        = MkStore { storeName        :: Column f String
                  , storeAddress     :: Column f Address
                  , storeId          :: Column f Int
                  }
        deriving (Generic, Frameable)
    deriving instance Show (Row Store)
:}

In this example, we assume that the @storeId@ column is unique. We will
therefore use it as a key. All we need to do is derive an instance of `Indexable`:

>>> :set -XTypeFamilies
>>> :{
    instance Indexable Store where
        type Key Store = Int
        index = storeId
:}

As an example, let's build a dataframe of stores:

>>> :{
    stores = fromRows
           $ Vector.fromList 
           [ MkStore "Store A" (Addr "8712 1st Avenue") 787123745
           , MkStore "Store B" (Addr "90 2st Street")   188712313
           , MkStore "Store C" (Addr "109 3rd Street")  910823870
           ]
:}

Finally, we can look up @Store A@ by its unique ID using `Frame.lookup`:

>>> Frame.lookup 787123745 stores
Just (MkStore {storeName = "Store A", storeAddress = Addr "8712 1st Avenue", storeId = 787123745})

== Compound keys

Sometimes, it is preferable to identify rows through multiple columns. Again in
in analogy with databases, the key is a _compound key_.

Let's consider another example, that of movie actors:

>>> :{
    data Actor f
        = MkActor { actorFirstName   :: Column f String
                  , actorLastName    :: Column f String
                  , actorAge         :: Column f Int
                  }
        deriving (Generic, Frameable)
    deriving instance Show (Row Actor)
:}

In this case, we can identify actors by their first and last name, 
which creates a compound key:

>>> :{
    instance Indexable Actor where
        type Key Actor = (String, String)
        index :: Frame Actor -> Vector (Key Actor)
        index df = Vector.zipWith (,) (actorFirstName df) (actorLastName df)
:}

We define some data

>>> :{
    actors = fromRows
           $ Vector.fromList 
           [ MkActor "George" "Clooney" 63
           , MkActor "Brad"   "Pitt"    61
           , MkActor "George" "Takei"   87
           ]
:}

Finally, we can look up George Clooney's age using `at`:

>>> actors `at` ( ("George", "Clooney"), actorAge )
Just 63

-}