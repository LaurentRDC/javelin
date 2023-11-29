# Haskell implementation of labeled one-dimensional arrays

Packages in this repository implement series, or labeled one-dimensional arrays, and associated functions.

Like [`Data.Map.Strict`](https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html), series support efficient:

* random access by key ( $\mathcal{O}\left( \log n \right)$ ) ;
* slice by key ( $\mathcal{O}\left( \log n \right)$ ).

Like [`Data.Vector.Vector`](https://hackage.haskell.org/package/vector/docs/Data-Vector.html), series support efficient:

* random access by integer index ( $\mathcal{O}\left( 1 \right)$ );
* slice by integer index ( $\mathcal{O}\left( 1 \right)$ );
* numerical operations.

## Tutorial and documentation

A tutorial and interface documentation for the most recent published version are [available here](https://hackage.haskell.org/package/javelin). 

Locally, you can generate documentation for all packages using `haddock` like so:

```bash
cabal haddock javelin
cabal haddock javelin-io
cabal haddock javelin-finance
```

## Get involved!

Do not hesitate to make feature requests or report bugs via the [issue tracker](https://github.com/LaurentRDC/javelin/issues).

## Preliminary benchmarks

Looking up random integers:

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Map.Lazy|89.53 ns|1.269 μs|50.53 μs|1.115 ms|
|Data.Map.Strict|103.5 ns|1.497 μs|51.25 μs|1.153 ms|
|Data.Series|176.9 ns|2.164 μs|68.50 μs|1.287 ms|
|Data.Vector|188.8 ns|11.06 μs|924.4 μs|98.32 ms|
|Data.Series.Unboxed|182.8 ns|2.204 μs|68.94 μs|1.268 ms|
|Data.Vector.Unboxed|133.4 ns|4.446 μs|307.5 μs|29.40 ms|

Summing random integers:

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|44.80 ns|330.4 ns|3.868 μs|67.41 μs|1504 μs|20.14 ms|
|Data.Map.Strict|44.39 ns|331.8 ns|3.701 μs|70.92 μs|1770 μs|21.08 ms|
|Data.Series|31.21 ns|265.1 ns|2.594 μs|30.32 μs|665.9 μs|13.94 ms|
|Data.Vector|31.01 ns|261.7 ns|2.623 μs|27.30 μs|366.1 μs|5.814 ms|
|Data.Series.Unboxed|16.04 ns|71.59 ns|0.520 μs|5.053 μs|50.91 μs|0.608 ms|
|Data.Vector.Unboxed|17.40 ns|75.46 ns|0.564 μs|5.240 μs|51.34 μs|0.612 ms|

Averaging elements using 'Control.Foldl.mean' from the 'foldl' package:

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|175.8 ns|1636 ns|17.80 μs|191.2 μs|3.268 ms|64.55 ms|
|Data.Map.Strict|176.4 ns|1727 ns|18.55 μs|208.8 μs|2.826 ms|69.86 ms|
|Data.Series|50.17 ns|621.3 ns|6.488 μs|63.07 μs|0.832 ms|13.66 ms|
|Data.Vector|55.49 ns|610.3 ns|6.345 μs|63.08 μs|0.682 ms|7.606 ms|
|Data.Series.Unboxed|32.92 ns|450.7 ns|5.112 μs|52.97 μs|0.509 ms|5.353 ms|
|Data.Vector.Unboxed|30.86 ns|448.2 ns|5.343 μs|51.13 μs|0.507 ms|5.420 ms|

Concatenating with `(<>)`:

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|212.4 ns|2.818 μs|27.78 μs|0.274 ms|5.286 ms|67.65 ms|
|Data.Map.Strict|228.3 ns|2.348 μs|26.79 μs|0.279 ms|2.752 ms|40.90 ms|
|Data.Series|1469 ns|15.55 μs|164.6 μs|2.833 ms|44.03 ms|550.2 ms|
|Data.Vector|94.14 ns|0.605 μs|4.276 μs|0.048 ms|0.506 ms|5.337 ms|
|Data.Series.Unboxed|1574 ns|15.76 μs|168.5 μs|2.900 ms|53.45 ms|565.5 ms|
|Data.Vector.Unboxed|103.9 ns|0.122 μs|0.512 μs|0.004 ms|0.054 ms|0.609 ms|

Extracing a 10% chunk of the container:

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|72.12 ns|650.4 ns|5.679 μs|89.16 μs|1.238 ms|31.56 ms|
|Data.Map.Strict|77.67 ns|628.2 ns|5.714 μs|85.86 μs|1.200 ms|20.89 ms|
|Data.Series|177.4 ns|938.3 ns|9.071 μs|194.9 μs|2.913 ms|44.76 ms|
|Data.Series.Unboxed|186.7 ns|1024 ns|9.087 μs|189.3 μs|2.485 ms|42.68 ms|

You can run the benchmarks above by running:

```shell
# Run the benchmark
cabal bench comparison-containers --project=cabal.project.benchmarking
# Format the resulting CSV file
cabal run bench-report javelin/out.csv report.md
```