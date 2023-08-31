module Specular.Dom.Builder
  ( AttrValue(..)
  , Attrs
  , Builder
  , DynNode(..)
  , Event
  , EventType
  , Namespace
  , Node
  , Fragment
  , TagName
  , addEventListener
  , appendChild
  , appendChildToBody
  , attachFragment
  , attr
  , classes
  , comment
  , createDocumentFragment
  , detachFragment
  , elAttr
  , getChecked
  , getEnv
  , getParentNode
  , getValue
  , local
  , newDynNode
  , newFragment
  , populateBody
  , rawHtml
  , removeNode
  , replaceNode
  , runBuilderWithUserEnv
  , setAttributes
  , setChecked
  , setValue
  , text
  )
  where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Reader (ask, asks)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (info)
import Effect.Now (now)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Specular.Internal.RIO (RIO, rio, runRIO)
import Specular.Internal.RIO as RIO


newtype Builder env a = Builder (RIO (BuilderEnv env) a)

type BuilderEnv env =
  { parent :: Node
  , userEnv :: env
  }

derive newtype instance functorBuilder :: Functor (Builder env)
derive newtype instance applyBuilder :: Apply (Builder env)
derive newtype instance applicativeBuilder :: Applicative (Builder env)
derive newtype instance bindBuilder :: Bind (Builder env)
derive newtype instance monadBuilder :: Monad (Builder env)
derive newtype instance monadEffectBuilder :: MonadEffect (Builder env)

instance monadAskBuilder :: MonadAsk env (Builder env) where
  ask = _.userEnv <$> getEnv

instance monadReaderBuilder :: MonadReader env (Builder env) where
  local = local

local :: forall e r a. (e -> r) -> Builder r a -> Builder e a
local fn (Builder x) = Builder $ RIO.local (\env -> env { userEnv = fn env.userEnv }) x

mkBuilder :: forall env a. (BuilderEnv env -> Effect a) -> Builder env a
mkBuilder = Builder <<< rio

unBuilder :: forall env a. Builder env a -> RIO (BuilderEnv env) a
unBuilder (Builder f) = f

runBuilderWithUserEnv :: forall env a. env -> Node -> Builder env a -> Effect a
runBuilderWithUserEnv userEnv parent (Builder f) = do
  let env = { parent, userEnv }
  runRIO env f

getEnv :: forall env. Builder env (BuilderEnv env)
getEnv = Builder ask

setParent :: forall env. Node -> BuilderEnv env -> BuilderEnv env
setParent parent env = env { parent = parent }

getParentNode :: forall env. Builder env Node
getParentNode = Builder (asks _.parent)

data DynNode = DynNode (String -> Effect Unit)

newDynNode :: forall env. Builder env DynNode
newDynNode = do
  env <- getEnv
  slotNo <- liftEffect $ Ref.modify (_ + 1) slotCounter
  placeholderBefore <- liftEffect $ createTextNodeImpl $ "<" <> show slotNo <> ">"
  placeholderAfter <- liftEffect $ createTextNodeImpl $ "</" <> show slotNo <> ">"
  liftEffect $ appendChild placeholderBefore env.parent
  liftEffect $ appendChild placeholderAfter env.parent

  pure $ DynNode \str -> measured' slotNo do
    newNode <- createTextNodeImpl str
    removeAllBetween placeholderBefore placeholderAfter
    newNode `insertBefore` placeholderAfter
    where
      measured' slotNo = measured $ "text " <> show slotNo <> " set"

replaceNode :: DynNode -> String -> Effect Unit
replaceNode (DynNode replace) = replace

data Fragment = Fragment
  (Effect Unit) -- ^ attach
  (Effect Unit) -- ^ detach

attachFragment :: Fragment -> Effect Unit
attachFragment (Fragment attach _) = attach

detachFragment :: Fragment -> Effect Unit
detachFragment (Fragment _ detach) = detach

slotCounter :: Ref.Ref Int
slotCounter = unsafePerformEffect $ Ref.new 0

newFragment :: forall env a. Builder env a -> Builder env (Tuple Fragment a)
newFragment builder = do
  env <- getEnv
  slotNo <- liftEffect $ Ref.modify (_ + 1) slotCounter

  placeholderBefore <- liftEffect $ createTextNodeImpl $ "<" <> show slotNo <> ">"
  placeholderAfter <- liftEffect $ createTextNodeImpl $ "</" <> show slotNo <> ">"

  liftEffect $ appendChild placeholderBefore env.parent
  liftEffect $ appendChild placeholderAfter env.parent

  documentFragmentRef <- liftEffect $ Ref.new Nothing

  let
    build :: forall x. Builder env x -> Effect x
    build builder = measured' slotNo "set" do
      documentFragment <- createDocumentFragment
      built <- runBuilderWithUserEnv env.userEnv documentFragment builder
      Ref.write (Just documentFragment) documentFragmentRef
      pure built

    attach :: Effect Unit
    attach = measured' slotNo "attached" do
      mCurrentDocumentFragment <- Ref.read documentFragmentRef
      for_ mCurrentDocumentFragment (_ `insertBefore` placeholderAfter)
      -- after instering documentFragment becomes empty

    detach :: Effect Unit
    detach = measured' slotNo "detached" do
      currentDocumentFragment <- createDocumentFragment
      moveAllBetween placeholderBefore placeholderAfter currentDocumentFragment
      Ref.write (Just currentDocumentFragment) documentFragmentRef

    -- destroy :: Effect Unit
    -- destroy = measured' slotNo "destroyed" do
    --   removeAllBetween placeholderBefore placeholderAfter
    --   removeNode placeholderBefore
    --   removeNode placeholderAfter

    -- append :: forall a. Builder env a -> Effect (Tuple (Fragment (Builder env)) a)
    -- append builder = measured' slotNo "fragment appended" do
    --   fragment <- createDocumentFragment
    --   result <- runBuilderWithUserEnv env.userEnv fragment $ newFragment builder
    --   insertBefore fragment placeholderAfter
    --   pure result

  built <- liftEffect $ build builder

  pure $ Tuple (Fragment attach detach) built
    where
      measured' :: forall a m. Bind m ⇒ MonadEffect m ⇒ Int -> String → m a → m a
      measured' slotNo actionName = measured ("fragment " <> show slotNo <> " " <> actionName)

text :: forall env. String -> Builder env Unit
text str = mkBuilder \env -> do
  node <- createTextNodeImpl str
  appendChild node env.parent

rawHtml :: forall env. String -> Builder env Unit
rawHtml html = mkBuilder \env ->
  appendRawHtml html env.parent

elAttr :: forall a env. TagName -> Attrs -> Builder env a -> Builder env (Tuple Node a)
elAttr tagName attrs inner = do
  env <- getEnv
  node <- liftEffect $ createElementNS Nothing tagName
  liftEffect $ setAttributes node attrs
  result <- Builder $ RIO.local (setParent node) $ unBuilder inner
  liftEffect $ appendChild node env.parent
  pure $ Tuple node result

comment :: forall env. String -> Builder env Unit
comment str = mkBuilder \env -> do
  node <- createCommentNode str
  appendChild node env.parent

instance semigroupBuilder :: Semigroup a => Semigroup (Builder node a) where
  append = lift2 append

instance monoidBuilder :: Monoid a => Monoid (Builder node a) where
  mempty = pure mempty

populateBody :: forall a. Builder Unit a → Effect a
populateBody builder = do
  body <- documentBody
  runBuilderWithUserEnv unit body builder

type Attrs = Object AttrValue

data AttrValue = ClassNames String | AttrValue String

classes :: String -> Attrs
classes spaceSeparatedClassNames = "class" := ClassNames spaceSeparatedClassNames

attr ∷ String → String → Attrs
attr attrName attrValue =  attrName := AttrValue attrValue

instance Semigroup AttrValue where
  append (ClassNames classNames1) (ClassNames classNames2) = ClassNames $ classNames1 <> " " <> classNames2
  append _ second = second

instance Show AttrValue where
  show (ClassNames s) = s
  show (AttrValue s) = s

-- | Convenient syntax for building Attrs
infix 8 Object.singleton as :=

type TagName = String

-- | XML namespace URI.
type Namespace = String

-- | DOM node.
foreign import data Node :: Type

-- | DOM event.
foreign import data Event :: Type

-- | HTML event type, e.g. "click".
type EventType = String

-- | Register an event listener. Returns unregister action.
addEventListener :: EventType -> Node -> (Event -> Effect Unit) -> Effect Unit
addEventListener etype node callback = void $ addEventListenerImpl etype (measured (etype <> " event handled") <<< callback) node

createDocumentFragment :: Effect Node
createDocumentFragment = createDocumentFragmentImpl

-- | Create an element, optionally with namespace.
createElementNS :: Maybe Namespace -> TagName -> Effect Node
createElementNS (Just namespace) = createElementNSImpl namespace
createElementNS Nothing = createElementImpl

setAttributes :: Node -> Attrs -> Effect Unit
setAttributes node attrs = runEffectFn2 setAttributesImpl node (show <$> attrs)

appendChildToBody ::Node -> Effect Unit
appendChildToBody child = do
  body <- documentBody
  appendChild child body

-- | Append a chunk of raw HTML to the end of the node.
appendRawHtml :: String -> Node -> Effect Unit
appendRawHtml = appendRawHtmlImpl

-- | `removeAllBetween from to`
-- |
-- | Remove all nodes after `from` and before `to` from their
-- | parent. `from` and `to` are not removed.
-- |
-- | Assumes that `from` and `to` have the same parent,
-- | and `from` is before `to`.
removeAllBetween :: Node -> Node -> Effect Unit
removeAllBetween = removeAllBetweenImpl

createCommentNode ∷ String → Effect Node
createCommentNode = createCommentNodeImpl

logIndent :: Ref.Ref Int
logIndent = unsafePerformEffect $ Ref.new 0

measured :: forall a m. Bind m ⇒ MonadEffect m ⇒ String → m a → m a
measured actionName action = do
  start <- liftEffect now
  _ <- liftEffect $ Ref.modify (_ + 1) logIndent
  a <- action
  currentIndent <- liftEffect $ Ref.modify (_ - 1) logIndent
  stop <- liftEffect now
  info $ "[DOM] " <> repeatStr currentIndent "." <> actionName <> " in " <> show (unwrap (unInstant stop) - unwrap (unInstant start)) <> " ms"
  pure a
    where
      repeatStr i s
        | i <= 0 = ""
        | otherwise = s <> repeatStr (i - 1) s

foreign import documentBody :: Effect Node
foreign import removeNode :: Node -> Effect Unit
foreign import createTextNodeImpl :: String -> Effect Node
foreign import setTextImpl :: Node -> String -> Effect Unit
foreign import createDocumentFragmentImpl :: Effect Node
foreign import createElementNSImpl :: Namespace -> TagName -> Effect Node
foreign import createElementImpl :: TagName -> Effect Node
foreign import removeAttributesImpl :: Node -> Array String -> Effect Unit
foreign import parentNodeImpl :: (Node -> Maybe Node) -> Maybe Node -> Node -> Effect (Maybe Node)
foreign import insertBefore :: Node -> Node -> Effect Unit
foreign import appendChild :: Node -> Node -> Effect Unit
foreign import removeAllBetweenImpl :: Node -> Node -> Effect Unit
foreign import appendRawHtmlImpl :: String -> Node -> Effect Unit
foreign import moveAllBetween :: Node -> Node -> Node -> Effect Unit
foreign import addEventListenerImpl :: String -> (Event -> Effect Unit) -> Node -> Effect (Effect Unit)
foreign import preventDefault :: Event -> Effect Unit
foreign import innerHTML :: Node -> Effect String
foreign import createCommentNodeImpl :: String -> Effect Node
foreign import getValue :: Node -> Effect String
foreign import setValue :: Node -> String -> Effect Unit
foreign import getChecked :: Node -> Effect Boolean
foreign import setChecked :: Node -> Boolean -> Effect Unit
foreign import setAttributesImpl :: EffectFn2 Node (Object String) Unit
foreign import removeParentfulNode :: Node -> Effect Unit
foreign import removeChildOfParent :: Node -> Node -> Effect Unit
foreign import childNodes :: Node -> Effect (Array Node)

