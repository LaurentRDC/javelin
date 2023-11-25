# [Experimental] Haskell implementation of labeled one-dimensional arrays

:warning: This repository is in experimental development. Backwards compatibility may be broken at any moment. Feel free to make usage suggestions if you're interested in this topic!

Packages in this repository implement series, or labeled one-dimensional arrays.

Like [`Data.Map.Strict`](https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html), series support efficient:

* random access by key ( $\mathcal{O}\left( \log n \right)$ ) ;
* slice by key ( $\mathcal{O}\left( \log n \right)$ ).

Like [`Data.Vector.Vector`](https://hackage.haskell.org/package/vector/docs/Data-Vector.html), series support efficient:

* random access by integer index ( $\mathcal{O}\left( 1 \right)$ );
* slice by integer index ( $\mathcal{O}\left( 1 \right)$ );
* numerical operations.

## Tutorial and documentation

A tutorial and interface documentation for the most recent version on the `master` branch are [available here](https://laurentrdc.github.io/javelin/). 
A tutorial is presented in the `Data.Series.Tutorial` module.

You can generate documentation for all packages using `haddock` like so:

```bash
cabal haddock javelin
cabal haddock javelin-io
cabal haddock javelin-finance
```

## Preliminary benchmarks

Looking up random integers:

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Map.Lazy|84.09 ns|1.073 μs|44.24 μs|1.018 ms|
|Data.Map.Strict|99.60 ns|1.288 μs|47.03 μs|1.052 ms|
|Data.Series|152.5 ns|1.806 μs|61.03 μs|1.178 ms|
|Data.Vector|169.1 ns|10.12 μs|900.1 μs|99.62 ms|
|Data.Series.Unboxed|174.5 ns|2.067 μs|62.12 μs|1.191 ms|
|Data.Vector.Unboxed|114.9 ns|4.251 μs|284.7 μs|27.94 ms|

Summing random integers:

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|38.67 ns|306.6 ns|3.330 μs|59.70 μs|1275 μs|18.31 ms|
|Data.Map.Strict|38.53 ns|291.0 ns|3.520 μs|59.45 μs|1464 μs|19.06 ms|
|Data.Series|27.91 ns|232.8 ns|2.485 μs|27.24 μs|473.8 μs|12.47 ms|
|Data.Vector|27.29 ns|236.4 ns|2.402 μs|25.05 μs|298.7 μs|4.928 ms|
|Data.Series.Unboxed|13.22 ns|64.80 ns|0.476 μs|4.685 μs|45.31 μs|0.508 ms|
|Data.Vector.Unboxed|13.83 ns|62.11 ns|0.499 μs|4.842 μs|46.86 μs|0.508 ms|

Averaging elements using 'Control.Foldl.mean' from the 'foldl' package:

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|153.5 ns|1591 ns|16.55 μs|174.9 μs|2.967 ms|64.65 ms|
|Data.Map.Strict|159.8 ns|1589 ns|17.03 μs|182.2 μs|2.740 ms|67.65 ms|
|Data.Series|44.86 ns|585.1 ns|6.062 μs|60.04 μs|0.823 ms|14.02 ms|
|Data.Vector|43.97 ns|591.4 ns|6.138 μs|60.43 μs|0.641 ms|7.364 ms|
|Data.Series.Unboxed|30.84 ns|438.1 ns|4.877 μs|49.59 μs|0.495 ms|4.954 ms|
|Data.Vector.Unboxed|29.91 ns|438.5 ns|4.969 μs|49.55 μs|0.489 ms|4.963 ms|

Appending integers:

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|0.204 μs|2.407 μs|26.84 μs|0.265 ms|4.914 ms|65.72 ms|
|Data.Map.Strict|0.205 μs|2.468 μs|25.72 μs|0.267 ms|2.753 ms|41.35 ms|
|Data.Series|2.914 μs|29.37 μs|431.2 μs|11.45 ms|199.6 ms|2309 ms|
|Data.Vector|0.083 μs|0.457 μs|4.382 μs|0.045 ms|0.519 ms|5.389 ms|
|Data.Series.Unboxed|3.030 μs|29.94 μs|458.1 μs|10.37 ms|173.7 ms|1935 ms|
|Data.Vector.Unboxed|0.101 μs|0.120 μs|0.493 μs|0.004 ms|0.049 ms|0.746 ms|

Extracing a 10% chunk of the container:

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|70.72 ns|661.8 ns|5.993 μs|80.95 μs|1.024 ms|25.02 ms|
|Data.Map.Strict|77.05 ns|610.5 ns|5.952 μs|80.67 μs|0.923 ms|20.14 ms|
|Data.Series|182.8 ns|918.3 ns|9.246 μs|187.9 μs|2.198 ms|43.99 ms|
|Data.Series.Unboxed|171.3 ns|966.3 ns|8.778 μs|179.0 μs|2.177 ms|40.92 ms|

You can run the benchmarks above by running:

```shell
# Run the benchmark
cabal bench comparison-containers --project=cabal.project.benchmarking
# Format the resulting CSV file
cabal run bench-report javelin/out.csv report.md
```