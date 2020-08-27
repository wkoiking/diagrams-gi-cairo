
_diagrams-gi-cairo_ is a fork of [diagrams-cairo], a rendering backend 
for [diagrams-2.0], a powerful, flexible, declarative domain-specific 
language for creating vector graphics, using the [Haskell programming 
language][haskell].

[diagrams-cairo]: https://hackage.haskell.org/package/diagrams-cairo
[diagrams-2.0]: https://github.com/cchalmers/diagrams
[haskell]: http://www.haskell.org/haskellwiki/Haskell

_diagrams-gi-cairo_ is implemented using the [cairo] rendering engine and
is a fully-featured, officially supported backend for diagrams.

[cairo]: http://www.cairographics.org/

Difference from `diagrams-cairo` is as below:

* `diagrams-gi-cairo` depends on `gi-cairo` package instead of `cairo` package.
* `diagrams-gi-cairo` depends on `diagrams-2.0` and `geometry` package instead of `diagrams-1.X`. Although `diagrams-2.0` is still in development, it is a lot more performant compared to `diagrams-1.X`. 

# Installation Note

## Using stack

1.  Currently (as of 2020/08/27), ghc-8.10 is not available for Windows due to below issue:

    [https://www.stackage.org/blog/2020/08/ghc-8-10-2-windows-workaround]

    Hence rather `lts-16.10` (ghc-8.8.4) is specified as resolver.

2.  Currently (as of 2020/08/27), `gi-pango` can not be built with `haskell-gi-0.24` (which is in `lts-16.10`) due to below issue:

    [https://github.com/haskell-gi/haskell-gi/issues/298]

3.  Currently (as of 2020/08/27), `gi-cairo-render-0.0.1` in lts-16.10 is not compatible with `haskell-gi-0.24` due to below issue:

    [https://github.com/cohomology/gi-cairo-render/issues/3]

    Hence `gi-cairo-render-0.0.2` in the below repository is used instead:

    [https://github.com/thestr4ng3r/gi-cairo-render/tree/fix-boxedobject]

In summary, need to specify below extra-deps in stack.yaml to build this package:

~~~
resolver: lts-16.10

packages:
- '.'

extra-deps: # []
- gi-cairo-render
- geometry
- diagrams
- active
- monoid-extras
- gi-pangocairo-1.0.23
- haskell-gi-base-0.24.2
- haskell-gi-0.24.4
- gi-pango-1.0.23
- gi-harfbuzz-0.0.3
~~~
