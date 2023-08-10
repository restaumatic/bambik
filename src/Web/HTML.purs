module Web.HTML
  ( checkbox
  , radio
  , staticText
  , text
  , textInput
  , onClick
  )
  where

import Prelude hiding (zero)

import Control.Monad.Replace (newSlot, replaceSlot)
import Data.Invariant.Transformers.Scoped (Scoped(..))
import Data.Newtype (wrap)
import Data.Plus (pzero)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Specular.Dom.Browser (Attrs, Node, onDomEvent, (:=))
import Specular.Dom.Builder.Class (elAttr)
import Specular.Dom.Builder.Class as S
import Web (WebComponent, inside')

foreign import getTextInputValue :: Node -> Effect String
foreign import setTextInputValue :: Node -> String -> Effect Unit
foreign import getCheckboxChecked :: Node -> Effect Boolean
foreign import setCheckboxChecked :: Node -> Boolean -> Effect Unit

staticText :: forall a . String -> WebComponent (Scoped a)
staticText content = wrap $ const $ S.text content *> mempty

text :: WebComponent (Scoped String)
text = wrap \_ -> do
  slot <- newSlot
  pure $ \(Scoped _ text) -> (replaceSlot slot <<< S.text) text



textInput :: Attrs -> WebComponent (Scoped String)
textInput attrs = wrap \callback -> do
  Tuple node a <- elAttr "input" attrs (pure unit)
  onDomEvent "input" node \event -> do
    getTextInputValue node >>= callback <<< Scoped mempty -- TODO check!
  pure $ \(Scoped _ text) -> setTextInputValue node text

checkbox :: Attrs -> WebComponent Boolean
checkbox attrs = wrap \callback -> do
  Tuple node a <- elAttr "input" attrs (pure unit)
  onDomEvent "input" node \event -> do
    getCheckboxChecked node >>= callback
  pure $ setCheckboxChecked node

-- TODO
radio :: (Boolean -> Attrs) -> WebComponent Boolean
radio attrs = pzero # inside' "input" (\_ -> let enabled = false in ("type" := "radio") <> (if enabled then "checked" := "checked" else mempty) <> attrs enabled) \node callback -> do
  mempty
  -- setCheckboxChecked node value
  -- onDomEvent "change" node (\_ -> getCheckboxChecked node >>= callback)
-- radio :: forall a b f. Applicative f => (a -> Attrs) -> ComponentWrapper f a b
-- radio attrs = pzero # (inside "input" (\enabled -> ("type" := "checkbox") <> (if enabled then "checked" := "checked" else mempty) <> attrs enabled) \_ node -> do
--   domEventWithSample (\_ -> getCheckboxChecked node <#> \value -> { path: [], value }) "change" node)

-- TODO
onClick ∷ forall a. Node → (a -> Effect Unit) -> Effect Unit
onClick node callback = mempty -- void $ DOM.addEventListener "click" (\_ -> callback a) node

