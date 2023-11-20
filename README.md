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
