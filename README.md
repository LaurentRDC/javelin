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
```

## Get involved!

Do not hesitate to make feature requests or report bugs via the [issue tracker](https://github.com/LaurentRDC/javelin/issues).

## Preliminary benchmarks

Looking up random integers:

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Map.Lazy|81.65 ns|1.156 μs|50.23 μs|1.141 ms|
|Data.Map.Strict|104.7 ns|1.257 μs|53.12 μs|1.157 ms|
|Data.Series|154.0 ns|1.774 μs|66.44 μs|1.232 ms|
|Data.Vector|166.2 ns|9.844 μs|926.5 μs|104.6 ms|
|Data.Series.Unboxed|166.0 ns|2.168 μs|71.80 μs|1.210 ms|
|Data.Vector.Unboxed|129.3 ns|4.415 μs|302.0 μs|28.15 ms|

Summing random integers:

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|39.14 ns|299.7 ns|3.381 μs|60.09 μs|1253 μs|18.12 ms|
|Data.Map.Strict|47.02 ns|300.1 ns|3.797 μs|64.48 μs|1804 μs|19.70 ms|
|Data.Series|28.72 ns|242.4 ns|2.453 μs|49.82 μs|633.7 μs|12.19 ms|
|Data.Vector|28.75 ns|274.8 ns|2.379 μs|25.56 μs|316.8 μs|5.073 ms|
|Data.Series.Unboxed|13.71 ns|61.19 ns|0.475 μs|4.712 μs|47.00 μs|0.528 ms|
|Data.Vector.Unboxed|15.74 ns|60.16 ns|0.481 μs|4.669 μs|46.83 μs|0.555 ms|

Averaging elements using 'Control.Foldl.mean' from the 'foldl' package:

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|171.3 ns|1814 ns|18.87 μs|192.0 μs|3.596 ms|75.92 ms|
|Data.Map.Strict|166.9 ns|1637 ns|19.76 μs|198.6 μs|3.708 ms|85.80 ms|
|Data.Series|70.08 ns|578.8 ns|6.008 μs|59.40 μs|0.947 ms|14.85 ms|
|Data.Vector|45.17 ns|576.7 ns|6.137 μs|62.53 μs|0.738 ms|8.033 ms|
|Data.Series.Unboxed|31.09 ns|436.4 ns|4.795 μs|50.70 μs|0.492 ms|4.911 ms|
|Data.Vector.Unboxed|30.32 ns|449.8 ns|5.107 μs|53.83 μs|0.545 ms|5.526 ms|

Concatenating with `(<>)`:

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|216.2 ns|2.377 μs|26.54 μs|269.9 μs|5.046 ms|66.52 ms|
|Data.Map.Strict|222.9 ns|2.338 μs|26.83 μs|269.3 μs|2.750 ms|44.66 ms|
|Data.Series|1329 ns|12.94 μs|138.0 μs|2183 μs|45.92 ms|547.7 ms|
|Data.Vector|79.38 ns|0.513 μs|4.201 μs|42.95 μs|0.463 ms|5.453 ms|
|Data.Series.Unboxed|1402 ns|12.49 μs|141.2 μs|2459 μs|47.08 ms|541.6 ms|
|Data.Vector.Unboxed|131.6 ns|0.145 μs|0.504 μs|4.056 μs|0.051 ms|0.622 ms|

Extracing a 10% chunk of the container:

|Name|10|100|1000|10000|100000|1000000|
|---|---|---|---|---|---|---|
|Data.Map.Lazy|71.73 ns|606.5 ns|5.271 μs|83.03 μs|1.008 ms|25.93 ms|
|Data.Map.Strict|77.99 ns|613.4 ns|5.280 μs|77.27 μs|0.900 ms|21.02 ms|
|Data.Series|165.1 ns|904.0 ns|8.690 μs|173.3 μs|2.103 ms|41.11 ms|
|Data.Series.Unboxed|158.8 ns|905.9 ns|8.656 μs|176.0 μs|2.262 ms|39.76 ms|

You can run the benchmarks above by running:

```shell
# Run the benchmark
cabal bench comparison-containers --project=cabal.project.benchmarking
# Format the resulting CSV file
cabal run bench-report javelin/out.csv report.md
```