## Module PUI.SVG

SVG element oculars. `element` (via `el`) is namespace-aware: `svg` opens
the SVG namespace and its children inherit it, so `circle`/`path`/`text`
used inside an `svg` are created as SVG-namespaced nodes. `text` lives here
rather than in `PUI.HTML` because that name is the channel-fed text leaf
there; import this module qualified (`import PUI.SVG as SVG`) when a widget
needs both the HTML `text` leaf and the SVG `<text>` element.

#### `circle`

``` purescript
circle :: Ocular (PUI Web)
```

#### `path`

``` purescript
path :: Ocular (PUI Web)
```

#### `svg`

``` purescript
svg :: Ocular (PUI Web)
```

#### `text`

``` purescript
text :: Ocular (PUI Web)
```


