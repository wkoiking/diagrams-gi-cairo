
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

You need to specify below extra-deps in stack.yaml to build this package:

~~~
resolver: lts-17.0

packages:
- '.'

extra-deps: # []
- geometry
- diagrams
- active
- monoid-extras
- gi-pangocairo-1.0.23
~~~
