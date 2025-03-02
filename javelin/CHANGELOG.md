# Revision history for javelin

## Release 0.1.4.2

* Explicit support for `containers-0.8`.

## Release 0.1.4.1

* Explicit support for GHC 9.12

## Release 0.1.4.0

* Added the `toSeriesDuplicates` method to the `IsSeries` typeclass, to more easily convert to a series
  from a container that may contain duplicates.
* Added `IsSeries` instance for `Series`.

## Release 0.1.3.1

* Improved performance for the `Foldable` methods that use `Data.Foldable.foldMap'` under the hood.

## Release 0.1.3.0

* Improved performance for the `aggregateWith` function.
* Ensured that arguments are consumed in the expected order using `foldWith`.
* Documentation improvements.

## Release 0.1.2.0

* Fixed an issue where `Series` could be corrupted while using `aggregateWith`.

## Release 0.1.1.0

* Added the `Data.Series.Index.indexed` function
* Replace all INLINE pragmas for INLINABLE, which will improve compilation speed and performance.

## Release 0.1.0.0

* This is the first version of `javelin` and associated packages.
