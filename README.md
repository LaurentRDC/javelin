# Haskell implementation of labeled one-dimensional arrays

Packages in this repository implement series, or labeled one-dimensional arrays.

Like [`Data.Map.Strict`](https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html), series support efficient:

* random access by key ( $\mathcal{O}\left( \log n \right)$ ) ;
* slice by key ( $\mathcal{O}\left( \log n \right)$ ).

Like [`Data.Vector.Vector`](https://hackage.haskell.org/package/vector/docs/Data-Vector.html), series support efficient:

* random access by integer index ( $\mathcal{O}\left( 1 \right)$ );
* slice by integer index ( $\mathcal{O}\left( 1 \right)$ );
* numerical operations.

## Documentation

### User guide

A user guide is available in the `docs` directory.

### Haddocks

You can generate documentation for `javelin` or `javelin-io` using `haddock` like so:

```bash
cabal haddock javelin
cabal haddock javelin-io
```