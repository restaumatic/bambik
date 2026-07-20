## Module PUI.Web

The carrier: the `Web` monad (`StateT DOM Effect`) the whole algebra is
instantiated at for the browser, plus the DOM building blocks and FFI
the HTML vocabulary (`PUI.HTML`) and the design system (`PUI.MDC`) are
built from. No widgets live here — for the 1-1 HTML vocabulary see
`PUI.HTML`.

#### `DOM`

``` purescript
type DOM = { parent :: Node, sibling :: Node }
```

##### Instances
``` purescript
MonadState DOM Web
```

#### `Event`

``` purescript
data Event
```

#### `Node`

``` purescript
data Node
```

#### `Web`

``` purescript
newtype Web a
```

##### Instances
``` purescript
Functor Web
Apply Web
Applicative Web
Bind Web
Monad Web
MonadEffect Web
MonadState DOM Web
```

#### `addClass`

``` purescript
addClass :: Node -> String -> Effect Unit
```

#### `addEventListener`

``` purescript
addEventListener :: String -> Node -> (Event -> Effect Unit) -> Effect (Effect Unit)
```

#### `appendChild`

``` purescript
appendChild :: Node -> Node -> Effect Unit
```

#### `appendRawHtml`

``` purescript
appendRawHtml :: String -> Node -> Effect Node
```

#### `attachable`

``` purescript
attachable :: forall r. Web r -> Web { ensureAttached :: Effect Unit, ensureDetached :: Effect Unit, result :: r }
```

#### `attribute`

``` purescript
attribute :: String -> String -> Web Unit
```

#### `childNS`

``` purescript
childNS :: String -> String -> String
```

The namespace rule for `element`: an `svg` tag opens the SVG namespace;
every other element inherits its parent's.

#### `clazz`

``` purescript
clazz :: String -> Web Unit
```

#### `createElementNS`

``` purescript
createElementNS :: String -> String -> Effect Node
```

#### `createTextNode`

``` purescript
createTextNode :: String -> Effect Node
```

#### `documentBody`

``` purescript
documentBody :: Effect Node
```

#### `element`

``` purescript
element :: forall a. String -> Web a -> Web a
```

#### `elementsInRange`

``` purescript
elementsInRange :: Node -> Node -> Effect (Array Node)
```

#### `htmlNS`

``` purescript
htmlNS :: String
```

The two namespaces the DOM builder distinguishes; SVG needs its elements
created with `createElementNS`, or the browser treats them as unknown HTML.

#### `getChecked`

``` purescript
getChecked :: Node -> Effect Boolean
```

#### `getValue`

``` purescript
getValue :: Node -> Effect String
```

#### `isFocused`

``` purescript
isFocused :: Node -> Effect Boolean
```

#### `lastChild`

``` purescript
lastChild :: Node -> Effect Node
```

#### `namespaceURI`

``` purescript
namespaceURI :: Node -> Effect String
```

#### `onInputDebounced`

``` purescript
onInputDebounced :: Node -> Number -> (String -> Effect Unit) -> Effect Unit
```

#### `onClickXY`

``` purescript
onClickXY :: Node -> (Number -> Number -> Effect Unit) -> Effect Unit
```

Pointer-down emitter with coordinates mapped into the element's local
space (an SVG's viewBox units when present, CSS pixels otherwise) —
works for mouse, touch and pen alike.

#### `removeAllChildren`

``` purescript
removeAllChildren :: Node -> Effect Unit
```

#### `removeChild`

``` purescript
removeChild :: Node -> Node -> Effect Unit
```

#### `removeAttribute`

``` purescript
removeAttribute :: Node -> String -> Effect Unit
```

#### `removeClass`

``` purescript
removeClass :: Node -> String -> Effect Unit
```

#### `runDomInNode`

``` purescript
runDomInNode :: forall a. Node -> Web a -> Effect a
```

#### `selectedNode`

``` purescript
selectedNode :: String -> Effect Node
```

#### `setAttribute`

``` purescript
setAttribute :: Node -> String -> String -> Effect Unit
```

#### `setChecked`

``` purescript
setChecked :: Node -> Boolean -> Effect Unit
```

#### `setInnerHTML`

``` purescript
setInnerHTML :: Node -> String -> Effect Unit
```

#### `setTextNodeValue`

``` purescript
setTextNodeValue :: Node -> String -> Effect Unit
```

#### `setValue`

``` purescript
setValue :: Node -> String -> Effect Unit
```

#### `svgNS`

``` purescript
svgNS :: String
```

#### `uniqueId`

``` purescript
uniqueId :: Effect String
```


