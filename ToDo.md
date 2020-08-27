
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

# �o��

## Dependancy�̃C���X�g�[��

nightly-2020-08-23��ghc-8.10.2�Ȃ̂Ń_�������������Bi.e.,

* https://www.stackage.org/blog/2020/08/ghc-8-10-2-windows-workaround

lts-16.10��haskell-gi-0.23.1����[����Issue](https://github.com/haskell-gi/haskell-gi/issues/298)��
��gi-pango���r���h�ł��Ȃ����A

- haskell-gi-base-0.24.2
- haskell-gi-0.24.4
- gi-pango-1.0.23
- gi-harfbuzz-0.0.3

��extra-deps�Ɏw�肵�ĂȂ�Ƃ�gi-cairo-render�ȊO�͍s�������ȂƂ���܂ł������B

gi-cairo-render�͂܂�haskell-gi-0.24�ɑΉ����ĂȂ��悤���F

�i�Q�l�jBoxedObject���폜���ꂽ�B

* https://github.com/haskell-gi/haskell-gi/commit/9f26532eaefe9236f61e5136dcf135e2d6b422c5

�Ή�����v�����N�͂�������悤�����A�}�[�W����Ă��Ȃ��F

* https://github.com/cohomology/gi-cairo-render/pull/4/commits

���L��gi-cairo-render���g�����炢�����F

* https://github.com/thestr4ng3r/gi-cairo-render/tree/fix-boxedobject

## diagrams-gi-cairo

gi-cairo-render��gi-cairo-connector���g��

diagrams-gi-cairo
    (showLayout, createLayout, updateLayout)��Render���i�h��Lift������
    liftRender0 :: (Cairo -> IO a) -> Render a
    liftRender0 f = ask >>= \context -> liftIO (f context)

    GI.Cairo.Structs.Context.Context��GI.Cairo.Render.Types.Cairo�̈Ⴂ�́H

getContext :: Render GI.Cairo.Context
toRender :: (GI.Cairo.Context -> IO a) -> Render a
