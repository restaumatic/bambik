-- DOM API
module Web.Internal.DOM
  ( Event
  , Namespace
  , Node
  , TagName
  , addEventListener
  , appendChild
  , appendRawHtml
  , attr
  , createCommentNode
  , createDocumentFragment
  , createElement
  , createElementNS
  , createTextNode
  , documentBody
  , getChecked
  , getValue
  , insertAsFirstChild
  , insertBefore
  , moveAllNodesBetweenSiblings
  , removeAllNodesBetweenSiblings
  , setAttributes
  , setChecked
  , setTextNodeValue
  , setValue
  )
  where

import Prelude

import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object

attr ∷ String → String → Object String
attr attrName attrValue = Object.singleton attrName attrValue

type TagName = String

-- | XML namespace URI.
type Namespace = String

-- | DOMBuilder node.
foreign import data Node :: Type

-- | DOMBuilder event.
foreign import data Event :: Type

-- | HTML event type, e.g. "click".
type EventType = String

foreign import getValue :: Node -> Effect String
foreign import setValue :: Node -> String -> Effect Unit
foreign import getChecked :: Node -> Effect Boolean
foreign import setChecked :: Node -> Boolean -> Effect Unit
foreign import documentBody :: Effect Node
foreign import createTextNode :: String -> Effect Node
foreign import createDocumentFragment :: Effect Node
foreign import createElementNS :: Namespace -> TagName -> Effect Node
foreign import createElement :: TagName -> Effect Node
foreign import insertBefore :: Node -> Node -> Effect Unit
foreign import appendChild :: Node -> Node -> Effect Unit
foreign import removeAllNodesBetweenSiblings :: Node -> Node -> Effect Unit
foreign import appendRawHtml :: String -> Node -> Effect Node
foreign import moveAllNodesBetweenSiblings :: Node -> Node -> Node -> Effect Unit
foreign import addEventListener :: String -> Node -> (Event -> Effect Unit) -> Effect (Effect Unit)
foreign import createCommentNode :: String -> Effect Node
foreign import setAttributes :: Node -> Object String -> Effect Unit
foreign import insertAsFirstChild :: Node -> Node -> Effect Unit
foreign import setTextNodeValue :: Node -> String -> Effect Unit
