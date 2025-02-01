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

    -- * Merging dataframes    
    -- ** Zipping
    -- $zipping
    
    -- ** Merging by key
    -- $merging

) where

import Data.Frame as Frame hiding (These(..))
import Data.Functor.Identity (Identity)
import qualified Data.List
import Data.These (These(..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
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
             [ MkStudent "Albert" 12 'C'
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

** Limitations

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

** Simple keys

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
           [ MkStore "Store A" (Addr "8712 1st Avenue") 787123745
           , MkStore "Store B" (Addr "90 2st Street")   188712313
           , MkStore "Store C" (Addr "109 3rd Street")  910823870
           ]
:}

Finally, we can look up @Store A@ by its unique ID using `Frame.lookup`:

>>> Frame.lookup 787123745 stores
Just (MkStore {storeName = "Store A", storeAddress = Addr "8712 1st Avenue", storeId = 787123745})

** Compound keys

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
           [ MkActor "George" "Clooney" 63
           , MkActor "Brad"   "Pitt"    61
           , MkActor "George" "Takei"   87
           ]
:}

Finally, we can look up George Clooney's age using `at`:

>>> actors `at` ( ("George", "Clooney"), actorAge )
Just 63

-}

{- $zipping

The simplest way to combine dataframes is analogous to the `Data.List.zipWith` operation
for lists: two dataframes can be combined row-by-row, in order, using `zipRowsWith`.

Here is an example:

>>> data Race = Cat | Dog deriving Show
>>> :{
    data Pet f
        = MkPet { petName :: Column f String
                , petAge  :: Column f Int
                }
        deriving (Generic, Frameable)
    pets = fromRows
         [ MkPet "Milo"    10
         , MkPet "Litchi"  4
         , MkPet "Piccolo" 15
         , MkPet "Cloud"   3
         ]
:}

>>> :{
    data PetInfo f
        = MkPetInfo { petInfoName  :: Column f String
                    , petInfoRace  :: Column f Race
                    }
        deriving (Generic, Frameable)
    petInfos = fromRows
             [ MkPetInfo "Milo"    Cat
             , MkPetInfo "Cloud"   Cat
             , MkPetInfo "Piccolo" Dog
             , MkPetInfo "Litchi"  Dog
             ]
:}

>>> :{
    data PetSummary f
        = MkPetSummary { petSummaryName :: Column f String
                       , petSummaryAge  :: Column f Int
                       , petSummaryRace :: Column f Race
                       }
        deriving (Generic, Frameable)
    deriving instance Show (Row PetSummary)
:}

>>> :{
    putStrLn
        $ display
            $ zipRowsWith 
                (\(MkPet name age) (MkPetInfo _ race) -> MkPetSummary name age race)
                pets
                petInfos
:}
petSummaryName | petSummaryAge | petSummaryRace
-------------- | ------------- | --------------
        "Milo" |            10 |            Cat
      "Litchi" |             4 |            Cat
     "Piccolo" |            15 |            Dog
       "Cloud" |             3 |            Dog


Hmm this doesn't look right, if you manually inspect the two source dataframes.
This is because rows are combined in order. You may want to sort rows using 
`sortRowsBy` or `sortRowsByUnique`, before applying `zipRowsWith`:

>>> import Data.Function (on)
>>> :{
    putStrLn
        $ display
            $ zipRowsWith 
                (\(MkPet name age) (MkPetInfo _ race) -> MkPetSummary name age race)
                (sortRowsBy (compare `on` petName) pets)
                (sortRowsBy (compare `on` petInfoName) petInfos)
:}
petSummaryName | petSummaryAge | petSummaryRace
-------------- | ------------- | --------------
       "Cloud" |             3 |            Cat
      "Litchi" |             4 |            Dog
        "Milo" |            10 |            Cat
     "Piccolo" |            15 |            Dog

There is a more robust way to merge dataframes, if each dataframe has a natural
key (in the case above, pet names). See below.
-}

{- $merging

If you want to merge dataframes whose rows have a natural key (i.e. have an instance of `Indexable`), 
then you should take a look at `mergeWithStrategy`. 
In this function, for each key present in __either__ dataframe, 
a merging strategy is applied. This strategy encodes how the merge should proceed in three cases:

* The key is present in the left dataframe, but not the right;
* The key is present in the right dataframe, but not the left;
* The key is present in both dataframes.

Let's see how to make use of this functionality by combining the information
about containers being shipped. Unfortunately, the data is spotty, so
we will need to make decisions about missing data.

>>> :{
    data ContainerOrigin f
        = MkContainerOrigin { containerOriginId      :: Column f Int
                            , containerOriginCountry :: Column f String
                            }
        deriving (Generic, Frameable)
    instance Indexable ContainerOrigin where
        type Key ContainerOrigin = Int
        index = containerOriginId
    containerOrigins = fromRows
                     [ MkContainerOrigin 1 "Canada"
                     , MkContainerOrigin 2 "Mexico"
                     -- missing container origin for container #3
                     , MkContainerOrigin 4 "Poland"
                     , MkContainerOrigin 5 "N/A" -- bad data
                     ]
:}

>>> :{
    data ContainerDest f -- Container destination
        = MkContainerDest { containerDestId      :: Column f Int
                          , containerDestCountry :: Column f String
                          }
        deriving (Generic, Frameable)
    instance Indexable ContainerDest where
        type Key ContainerDest = Int
        index = containerDestId
    containerDests = fromRows
                   [ MkContainerDest 1 "Japan"
                   , MkContainerDest 2 "Canada"
                   , MkContainerDest 3 "USA"
                   -- missing container destination for #4 
                   , MkContainerDest 5 "France"
                   ]
:}

We will first start by merging the dataframes only when we have complete data 
(i.e. an inner join). We first define the shape of the resulting dataframe:

>>> :{
    data ContainerJourney f
        = MkContainerJourney { containerJourneyId   :: Column f Int
                             , containerJourneyOrig :: Column f String
                             , containerJourneyDest :: Column f String
                             }
        deriving (Generic, Frameable)
    deriving instance Show (Row ContainerJourney)
:}

and define our merging strategy. The three row-wise merge cases are handled by the constructor
fro "Data.These", namely the constructors:

* `This`: The key is present in the left dataframe, but not the right;
* `That`: The key is present in the right dataframe, but not the left;
* `These`: The key is present in both dataframes (not to be confused with `These` the type).

In the simplest case, we only care about keys present in both dataframe (`These`)
>>> :{
    completeDataStrategy :: Int -> These (Row ContainerOrigin) (Row ContainerDest) -> Maybe (Row ContainerJourney)
    completeDataStrategy containerId (These (MkContainerOrigin _ origin) (MkContainerDest _ dest))
        = Just $ MkContainerJourney containerId origin dest
    completeDataStrategy _ _ = Nothing -- not enough data
:}

Sidenote: @completeDataStrategy@ is equivalent to `matchedStrategy`. We re-defined it for illustrative purposes.
>>> :{
    putStrLn
        $ display
            $ mergeWithStrategy 
                completeDataStrategy
                containerOrigins
                containerDests
:}  
containerJourneyId | containerJourneyOrig | containerJourneyDest
------------------ | -------------------- | --------------------
                 1 |             "Canada" |              "Japan"
                 2 |             "Mexico" |             "Canada"
                 5 |                "N/A" |             "France"

As expected, we do not have enough information to reconstruct the journey for container 3 (no known origin)
and container 4 (no known destination).
However, container 5's origin isn't valid data. We can further tweak the merge strategy to take this into account.

We crudely define what is a valid country name:

>>> validCountry name = not (name == "N/A")

and we can now define a new merging strategy. Returning a `Nothing` result from a merging strategy
effectively cancels the merge:

>>> :{
    completeDataStrategy' :: Int -> These (Row ContainerOrigin) (Row ContainerDest) -> Maybe (Row ContainerJourney)
    completeDataStrategy' containerId (These (MkContainerOrigin _ origin) (MkContainerDest _ dest))
        | validCountry origin && validCountry dest = Just $ MkContainerJourney containerId origin dest
        | otherwise                                = Nothing 
    completeDataStrategy' _ _ = Nothing -- not enough data
:}

>>> :{
    putStrLn
        $ display
            $ mergeWithStrategy 
                completeDataStrategy'
                containerOrigins
                containerDests
:}  
containerJourneyId | containerJourneyOrig | containerJourneyDest
------------------ | -------------------- | --------------------
                 1 |             "Canada" |              "Japan"
                 2 |             "Mexico" |             "Canada"

What if we can tolerate some missing data? Here, we only care where the container is going, but not
necessarily its origin. Let's redefine our resulting dataframe to take this into account:

>>> :{
    data PartialContainerJourney f
        = MkPartialContainerJourney { partialContainerJourneyId   :: Column f Int
                                    , partialContainerJourneyOrig :: Column f (Maybe String)
                                    , partialContainerJourneyDest :: Column f String
                                    }
        deriving (Generic, Frameable)
    deriving instance Show (Row PartialContainerJourney)
:}

>>> :{
    maybeOriginStrategy :: Int -> These (Row ContainerOrigin) (Row ContainerDest) -> Maybe (Row PartialContainerJourney)
    maybeOriginStrategy containerId (These (MkContainerOrigin _ origin) (MkContainerDest _ dest))
        | validCountry origin && validCountry dest = Just $ MkPartialContainerJourney containerId (Just origin) dest
        | validCountry dest                        = Just $ MkPartialContainerJourney containerId Nothing       dest
        | otherwise                                = Nothing
    maybeOriginStrategy containerId (That (MkContainerDest _ dest)) 
                                                   = Just $ MkPartialContainerJourney containerId Nothing       dest
    maybeOriginStrategy _           (This _)       = Nothing -- we require a destination
:}

>>> :{
    putStrLn
        $ display
            $ mergeWithStrategy 
                maybeOriginStrategy
                containerOrigins
                containerDests
:}
partialContainerJourneyId | partialContainerJourneyOrig | partialContainerJourneyDest
------------------------- | --------------------------- | ---------------------------
                        1 |               Just "Canada" |                     "Japan"
                        2 |               Just "Mexico" |                    "Canada"
                        3 |                     Nothing |                       "USA"
                        5 |                     Nothing |                    "France"
-}