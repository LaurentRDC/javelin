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
|Data.Map.Lazy|84.52 ns|1.165 μs|44.79 μs|0.975 ms|
|Data.Map.Strict|108.9 ns|1.422 μs|47.82 μs|1.023 ms|
|Data.Series|171.4 ns|2.040 μs|66.36 μs|1.194 ms|
|Data.Vector|170.8 ns|10.89 μs|948.8 μs|101.4 ms|
|Data.Series.Unboxed|161.5 ns|2.185 μs|65.74 μs|1.169 ms|
|Data.Vector.Unboxed|119.5 ns|4.079 μs|288.9 μs|27.50 ms|

Summing random integers:

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|38.10 ns|330.4 ns|3.725 μs|58.74 μs|1321 μs|18.02 ms|
|Data.Map.Strict|40.13 ns|296.4 ns|3.376 μs|60.93 μs|1411 μs|19.33 ms|
|Data.Series|29.27 ns|238.2 ns|2.356 μs|29.69 μs|487.0 μs|12.22 ms|
|Data.Vector|27.34 ns|239.4 ns|2.359 μs|26.91 μs|329.5 μs|4.793 ms|
|Data.Series.Unboxed|13.51 ns|61.87 ns|0.475 μs|4.794 μs|48.32 μs|0.468 ms|
|Data.Vector.Unboxed|14.64 ns|66.83 ns|0.530 μs|4.972 μs|47.48 μs|0.519 ms|

Averaging elements using 'Control.Foldl.mean' from the 'foldl' package:

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|170.9 ns|1552 ns|16.51 μs|178.0 μs|3.299 ms|76.16 ms|
|Data.Map.Strict|170.8 ns|1563 ns|16.61 μs|179.5 μs|3.581 ms|76.56 ms|
|Data.Series|42.99 ns|573.4 ns|5.909 μs|62.17 μs|0.776 ms|13.28 ms|
|Data.Vector|42.01 ns|574.8 ns|5.898 μs|58.24 μs|0.644 ms|7.137 ms|
|Data.Series.Unboxed|29.70 ns|423.5 ns|4.728 μs|47.49 μs|0.476 ms|4.785 ms|
|Data.Vector.Unboxed|30.51 ns|426.7 ns|5.096 μs|47.57 μs|0.476 ms|4.814 ms|

Appending integers:

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|201.6 ns|2.128 μs|24.96 μs|245.8 μs|4.241 ms|64.31 ms|
|Data.Map.Strict|201.9 ns|2.152 μs|23.66 μs|244.8 μs|2.641 ms|39.21 ms|
|Data.Series|847.6 ns|875.7 ns|12.02 μs|12.83 μs|143.0 μs|155.4 μs|1777 μs|1703 μs|38.56 ms|23.37 ms|433.0 ms|268.9 ms|
|Data.Vector|83.12 ns|0.477 μs|4.227 μs|44.07 μs|0.526 ms|5.158 ms|
|Data.Vector.Unboxed|78.04 ns|0.477 μs|4.242 μs|41.83 μs|0.492 ms|5.168 ms|

Extracing a 10% chunk of the container:

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|70.39 ns|603.9 ns|5.164 μs|86.00 μs|0.962 ms|26.27 ms|
|Data.Map.Strict|74.12 ns|695.3 ns|5.546 μs|69.44 μs|0.878 ms|20.27 ms|
|Data.Series|168.6 ns|162.3 ns|998.8 ns|885.0 ns|9.029 μs|9.109 μs|166.8 μs|170.4 μs|1.946 ms|1.931 ms|40.90 ms|38.40 ms|

You can run the benchmarks above by running:

```shell
# Run the benchmark
cabal bench comparison-containers --project=cabal.project.benchmarking
# Format the resulting CSV file
cabal run bench-report javelin/out.csv report.md
```