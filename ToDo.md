
_diagrams-gi-cairo_ is a rendering backend for [diagrams], a powerful,
flexible, declarative domain-specific language for creating vector graphics,
using the [Haskell programming language][haskell].

[diagrams]: http://projects.haskell.org/diagrams/
[haskell]: http://www.haskell.org/haskellwiki/Haskell

_diagrams-gi-cairo_ is implemented using the [cairo] rendering engine and
is a fully-featured, officially supported backend for diagrams.

[cairo]: http://www.cairographics.org/

# Dependency

gi-cairo-render
gi-pangocairo
gi-cairo-connector

```
cabal update && cabal install gtk2hs-buildtools diagrams-cairo
```

# 経過

## Dependancyのインストール

nightly-2020-08-23はghc-8.10.2なのでダメそうだった。i.e.,

* https://www.stackage.org/blog/2020/08/ghc-8-10-2-windows-workaround

lts-16.10はhaskell-gi-0.23.1だと[このIssue](https://github.com/haskell-gi/haskell-gi/issues/298)で
でgi-pangoがビルドできないが、

- haskell-gi-base-0.24.2
- haskell-gi-0.24.4
- gi-pango-1.0.23
- gi-harfbuzz-0.0.3

をextra-depsに指定してなんとかgi-cairo-render以外は行けそうなところまでいった。

gi-cairo-renderはまだhaskell-gi-0.24に対応してないようだ：

（参考）BoxedObjectが削除された。

* https://github.com/haskell-gi/haskell-gi/commit/9f26532eaefe9236f61e5136dcf135e2d6b422c5

対応するプルリクはもうあるようだが、マージされていない：

* https://github.com/cohomology/gi-cairo-render/pull/4/commits

下記のgi-cairo-renderを使ったらいけた：

* https://github.com/thestr4ng3r/gi-cairo-render/tree/fix-boxedobject

## diagrams-gi-cairo

gi-cairo-renderとgi-cairo-connectorを使う

diagrams-gi-cairo
    (showLayout, createLayout, updateLayout)をRenderモナドにLiftしたい
    liftRender0 :: (Cairo -> IO a) -> Render a
    liftRender0 f = ask >>= \context -> liftIO (f context)

    GI.Cairo.Structs.Context.ContextとGI.Cairo.Render.Types.Cairoの違いは？

getContext :: Render GI.Cairo.Context
toRender :: (GI.Cairo.Context -> IO a) -> Render a
