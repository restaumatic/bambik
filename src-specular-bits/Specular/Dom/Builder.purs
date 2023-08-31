module Specular.Dom.Builder
  ( AttrValue(..)
  , Attrs
  , Builder
  , DetachableDocumentFragment
  , Event
  , EventType
  , Namespace
  , Node
  , TagName
  , WritableTextNode(..)
  , addEventListener
  , appendChildToBody
  , attachDocumentFragment
  , attr
  , classes
  , comment
  , createDetachableDocumentFragment
  , createDetachableRootDocumentFragment
  , createDocumentFragment
  , createWritableTextNode
  , detachDocumentFragment
  , elAttr
  , getChecked
  , getEnv
  , getParentNode
  , getValue
  , local
  , populateBody
  , rawHtml
  , removeNode
  , runBuilderWithUserEnv
  , setAttributes
  , setChecked
  , setValue
  , writeToTextNode
  )
  where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Reader (ask, asks)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Data.DateTime.Instant (unInstant)
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
import Unsafe.Coerce (unsafeCoerce)


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

--

data WritableTextNode = WritableTextNode (String -> Effect Unit)

createWritableTextNode :: forall env. Builder env WritableTextNode
createWritableTextNode = do
  env <- getEnv
  slotNo <- liftEffect $ Ref.modify (_ + 1) slotCounter
  liftEffect $ measured' slotNo "created" do
    placeholderBefore <- newPlaceholderBefore slotNo
    placeholderAfter <- newPlaceholderAfter slotNo
    appendChild placeholderBefore env.parent
    appendChild placeholderAfter env.parent
    pure $ WritableTextNode \str -> measured' slotNo "written" do
      newNode <- createTextNode str
      removeAllNodesBetweenSiblings placeholderBefore placeholderAfter
      newNode `insertBefore` placeholderAfter
      where
        measured' :: forall b m. MonadEffect m => Int -> String → m b → m b
        measured' slotNo actionName = measured $ "text node " <> show slotNo <> " " <> actionName

writeToTextNode :: WritableTextNode -> String -> Effect Unit
writeToTextNode (WritableTextNode write) = write

data DetachableDocumentFragment = DetachableDocumentFragment
  (Effect Unit) -- ^ attach
  (Effect Unit) -- ^ detach

createDetachableRootDocumentFragment :: forall env a. Builder env a -> (a -> Effect Unit) -> Builder env Unit
createDetachableRootDocumentFragment builder initializer = do
  Tuple fragment built <- createDetachableDocumentFragment' true builder
  measured "initialization event handled" $ liftEffect $ do
    initializer built
    attachDocumentFragment fragment

createDetachableDocumentFragment :: forall env a. Builder env a -> Builder env (Tuple DetachableDocumentFragment a)
createDetachableDocumentFragment = createDetachableDocumentFragment' false

createDetachableDocumentFragment' :: forall env a. Boolean -> Builder env a -> Builder env (Tuple DetachableDocumentFragment a)
createDetachableDocumentFragment' isRoot builder = do
  env <- getEnv
  slotNo <- liftEffect $ Ref.modify (_ + 1) slotCounter
  liftEffect $ measured' slotNo "created" do

    placeholderBefore <- newPlaceholderBefore slotNo
    placeholderAfter <- newPlaceholderAfter slotNo

    if isRoot
      then insertAsFirstChild placeholderBefore env.parent
      else appendChild placeholderBefore env.parent
    if isRoot
      then insertAsLastChild placeholderAfter env.parent
      else appendChild placeholderAfter env.parent

    initialDocumentFragment <- createDocumentFragment
    built <- runBuilderWithUserEnv env.userEnv initialDocumentFragment builder

    documentFragmentRef <- Ref.new initialDocumentFragment

    let
      attach :: Effect Unit
      attach = measured' slotNo "attached" do
        when isRoot do
          removeAllNodesBetweenSiblings placeholderBefore placeholderAfter
        documentFragment <- Ref.modify' (\fragment -> { state: unsafeCoerce unit, value: fragment}) documentFragmentRef
          -- inserting documentFragment makes it empty but just in case not keeping reference to it while it's not needed
        documentFragment `insertBefore` placeholderAfter

      detach :: Effect Unit
      detach = measured' slotNo "detached" do
        documentFragment <- createDocumentFragment
        moveAllNodesBetweenSiblings placeholderBefore placeholderAfter documentFragment
        Ref.write documentFragment documentFragmentRef

    pure $ Tuple (DetachableDocumentFragment attach detach) built
    where
      measured' :: forall b m. MonadEffect m => Int -> String → m b → m b
      measured' slotNo actionName = measured ("document fragment " <> show slotNo <> " " <> actionName)

attachDocumentFragment :: DetachableDocumentFragment -> Effect Unit
attachDocumentFragment (DetachableDocumentFragment attach _) = attach

detachDocumentFragment :: DetachableDocumentFragment -> Effect Unit
detachDocumentFragment (DetachableDocumentFragment _ detach) = detach

--

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

-- | Create an element, optionally with namespace.
createElementNS :: Maybe Namespace -> TagName -> Effect Node
createElementNS (Just namespace) = createElementNSImpl namespace
createElementNS Nothing = createElement

setAttributes :: Node -> Attrs -> Effect Unit
setAttributes node attrs = runEffectFn2 setAttributesImpl node (show <$> attrs)

appendChildToBody ::Node -> Effect Unit
appendChildToBody child = do
  body <- documentBody
  appendChild child body

-- | `removeAllNodesBetweenSiblings from to`
-- |
-- | Remove all nodes after `from` and before `to` from their
-- | parent. `from` and `to` are not removed.
-- |
-- | Assumes that `from` and `to` have the same parent,
-- | and `from` is before `to`.

logIndent :: Ref.Ref Int
logIndent = unsafePerformEffect $ Ref.new 0

measured :: forall a m. MonadEffect m ⇒ String → m a → m a
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

slotCounter :: Ref.Ref Int
slotCounter = unsafePerformEffect $ Ref.new 0

newPlaceholderBefore ∷ ∀ (a95 ∷ Type). Show a95 ⇒ a95 → Effect Node
-- newPlaceholderBefore slotNo = createTextNode $ "<" <> show slotNo <> ">"
newPlaceholderBefore _ = createTextNode ""

newPlaceholderAfter ∷ ∀ (a100 ∷ Type). Show a100 ⇒ a100 → Effect Node
-- newPlaceholderAfter slotNo = createTextNode $ "</" <> show slotNo <> ">"
newPlaceholderAfter _ = createTextNode ""
foreign import removeNode :: Node -> Effect Unit

--  private

foreign import documentBody :: Effect Node
foreign import createTextNode :: String -> Effect Node
foreign import createDocumentFragment :: Effect Node
foreign import createElementNSImpl :: Namespace -> TagName -> Effect Node
foreign import createElement :: TagName -> Effect Node
foreign import insertBefore :: Node -> Node -> Effect Unit
foreign import appendChild :: Node -> Node -> Effect Unit
foreign import removeAllNodesBetweenSiblings :: Node -> Node -> Effect Unit
foreign import appendRawHtml
 :: String -> Node -> Effect Unit
foreign import moveAllNodesBetweenSiblings :: Node -> Node -> Node -> Effect Unit
foreign import addEventListenerImpl :: String -> (Event -> Effect Unit) -> Node -> Effect (Effect Unit)
foreign import createCommentNode :: String -> Effect Node
foreign import getValue :: Node -> Effect String
foreign import setValue :: Node -> String -> Effect Unit
foreign import getChecked :: Node -> Effect Boolean
foreign import setChecked :: Node -> Boolean -> Effect Unit
foreign import setAttributesImpl :: EffectFn2 Node (Object String) Unit
foreign import insertAsFirstChild :: Node -> Node -> Effect Unit
foreign import insertAsLastChild :: Node -> Node -> Effect Unit

