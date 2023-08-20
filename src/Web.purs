module Web
  ( Component
  , Listener
  , Widget
  , button
  , button'
  , chars
  , checkbox
  , div
  , div'
  , element
  , element'
  , label
  , label'
  , module Data.Profunctor.Plus
  , onClick
  , radio
  , runComponent
  , runMainComponent
  , span
  , span'
  , text
  , textInput
  )
  where

import Prelude hiding (zero)

import Data.Either (Either(..))
import Data.Invariant.Transformers.Changed (Change(..), Changed(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Plus (class ProfunctorZero, class ProfunctorPlus, proplus, proplusfirst, proplussecond, pzero, (<^), (^), (^>))
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Effect.Uncurried (runEffectFn2)
import Specular.Dom.Builder (Attrs, Builder, Node, TagName, addEventListener, attr, elAttr, getChecked, getValue, newSlot, onDomEvent, replaceSlot, runMainBuilderInBody, setAttributesImpl, setChecked, setValue)
import Specular.Dom.Builder as Builder

class Monad m <= MonadBuilder m where
  mempty :: forall a . Monoid a => m a

class MonadRef m where
  new :: forall a. a -> m (Ref.Ref a)
  read :: forall a. Ref.Ref a -> m a
  write :: forall a. a -> Ref.Ref a -> m Unit

class MonadSlot m where
  newSlot :: m (Slot m)
  replaceSlot :: forall m a. Slot m -> m a -> Effect a

instance MonadBuilder (Builder env) where
  mempty = pure mempty

instance MonadRef (Builder env) where
  new = liftEffect <<< Ref.new
  read = liftEffect <<< Ref.read
  write a ref = liftEffect $ Ref.write a ref


-- instance Monoid a => Monoid (Builder node a) where
--   mempty = pure mempty

newtype Widget m i o = Widget ((o -> Effect Unit) -> m (i -> m Unit))

instance Monad m => Profunctor (Widget m) where
  dimap post pre c = Widget \callback -> do
    f <- unwrapWidget c $ callback <<< pre
    pure $ f <<< post

instance MonadRef m => Strong (Widget m) where
  first c = Widget \abcallback -> do
    bref <- new Nothing
    update <- unwrapWidget c \a -> do
      mb <- read bref
      maybe mempty (\b -> abcallback $ Tuple a b) mb
    pure $ \ab -> do
      write (Just (snd ab)) bref
      update $ fst ab
  second c = Widget \abcallback -> do
    aref <- new Nothing
    update <- unwrapWidget c \b -> do
      ma <- read aref
      maybe mempty (\a -> abcallback $ Tuple a b) ma
    pure $ \ab -> do
      write (Just (fst ab)) aref
      update $ snd ab

instance MonadSlot m => Choice (Widget m) where
  left c = Widget \abcallback -> do
    slot <- newSlot
    mUpdateRef <- new Nothing
    pure \aorb -> case aorb of
      Left a -> do
        mUpdate <- read mUpdateRef
        update <- case mUpdate of
          Just update -> pure update
          Nothing -> do
            newUpdate <- replaceSlot slot $ unwrapWidget c (abcallback <<< Left)
            write (Just newUpdate) mUpdateRef
            pure newUpdate
        update a
      _ -> do
        void $ replaceSlot slot $ pure unit
        write Nothing mUpdateRef
        -- interestingly, theoretically, here we could call:
        -- abcallback userInput
        -- I don't know whether it would be right, though.
        -- Is that stil relevant question?
  right c = Widget \abcallback -> do
    slot <- newSlot
    mUpdateRef <- liftEffect $ Ref.new Nothing
    pure \aorb -> case aorb of
      Right b -> do
        mUpdate <- liftEffect $ Ref.read mUpdateRef
        update <- case mUpdate of
          Just update -> pure update
          Nothing -> do
            newUpdate <- liftEffect $ replaceSlot slot $ unwrapWidget c (abcallback <<< Right)
            liftEffect $ Ref.write (Just newUpdate) mUpdateRef
            pure newUpdate
        update $ b
      _ -> do
        void $ liftEffect $ replaceSlot slot $ pure unit
        Ref.write Nothing mUpdateRef
        -- interestingly, theoretically, here we could call:
        -- abcallback userInput
        -- I don't know whether it would be right, though.
        -- Is that stil relevant question?

instance MonadRef m => ProfunctorPlus (Widget m) where
  proplus c1 c2 = Widget \updateParent -> do
    -- TODO how to get rid of this ref?
    mUpdate2Ref <- new Nothing
    update1 <- unwrapWidget c1 \a -> do
      mUpdate2 <- read mUpdate2Ref
      let update2 = maybe mempty identity mUpdate2
      update2 a
      updateParent a
    update2 <- unwrapWidget c2 \a -> do
      update1 a
      updateParent a
    liftEffect $ write (Just update2) mUpdate2Ref
    pure \a -> do
      update1 a
      update2 a
  proplusfirst c1 c2 = Widget \updateParent -> do
    update1 <- unwrapWidget c1 updateParent
    update2 <- unwrapWidget c2 mempty
    pure \a -> do
      update1 a
      update2 a
  proplussecond c1 c2 = Widget \updateParent -> do
    update1 <- unwrapWidget c1 mempty
    update2 <- unwrapWidget c2 updateParent
    pure \a -> do
      update1 a
      update2 a

instance MonadBuilder m => ProfunctorZero (Widget m) where
  pzero = Widget mempty

-- Widgets

text :: forall a. Widget (Changed String) (Changed a)
text = Widget \_ -> do
  slot <- newSlot
  pure $ case _ of
    Changed None _ -> pure unit
    Changed _ s -> replaceSlot slot $ Builder.text s

chars :: forall a b. String -> Widget a b
chars s = Widget \_ -> do
  Builder.text s
  pure $ mempty

element :: forall a b. TagName -> Attrs -> (a -> Attrs) -> Listener a -> Widget a b -> Widget a b
element tagName attrs dynAttrs listener c = Widget \callback -> do
    maRef <- liftEffect $ Ref.new Nothing
    Tuple node update <- elAttr tagName attrs $ unwrapWidget c callback
    liftEffect $ listener node (Ref.read maRef)
    pure \a -> do
      Ref.write (Just a) maRef
      liftEffect $ runEffectFn2 setAttributesImpl node (show <$> attrs <> dynAttrs a)
      update a

element' :: forall a b. TagName -> Widget a b -> Widget a b
element' tagName = element tagName mempty mempty mempty

div' :: forall a b. Widget a b -> Widget a b
div' = element' "div"

div :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect (Maybe a) -> Effect Unit) -> Widget a b -> Widget a b
div = element "div"

span' :: forall a b. Widget a b -> Widget a b
span' = element' "span"

span :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect (Maybe a) -> Effect Unit) -> Widget a b -> Widget a b
span = element "span"

label' :: forall a b. Widget a b -> Widget a b
label' = element' "label"

label :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect (Maybe a) -> Effect Unit) -> Widget a b -> Widget a b
label = element "label"

button :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect (Maybe a) -> Effect Unit) -> Widget a b -> Widget a b
button = element "button"

button' :: forall a b. Widget a b -> Widget a b
button' = element' "button"

-- Components
-- ... are Widgets that have same input and output type and carry metadata about the scope of a change in input and output.
-- Components preserve Profunctor, Strong, Choice, ProfunctorPlus and ProfunctorZero instances of Widget.

type Component m a = Widget m (Changed a) (Changed a)

textInput :: Attrs -> Component String
textInput attrs = component $ widget \callback -> do
  Tuple node _ <- elAttr "input" attrs (pure unit)
  onDomEvent "input" node $ const $ getValue node >>= callback
  pure $ setValue node

checkbox :: Attrs -> Component Boolean
checkbox attrs = component $ widget \callback -> do
  Tuple node _ <- elAttr "input" (attr "type" "checkbox" <> attrs) (pure unit)
  onDomEvent "input" node $ const $ getChecked node >>= callback
  pure $ setChecked node

-- input:
-- Nothing -> turn off button
-- Just a -> turn on (if turned off) button and remember a
-- output:
-- Nothing -> button was clicked but button doesn't remember any a
-- Just a -> button was clicked and button does remember an a
radio :: forall a. Attrs -> Component (Maybe a)
radio attrs = component $ widget \callbacka -> do
  maRef <- liftEffect $ Ref.new Nothing
  Tuple node _ <- elAttr "input" (attr "type" "radio" <> attrs) (pure unit)
  onDomEvent "change" node $ const do
    ma <- Ref.read maRef
    callbacka ma
  pure case _ of
    Nothing -> setChecked node false
    Just a -> do
      Ref.write (Just a) maRef
      setChecked node true

-- Listeners

type Listener a = Node -> Effect (Maybe a) -> Effect Unit

onClick ∷ forall a. (a -> Effect Unit) -> Listener (Changed a)
onClick callback node emsa = void $ addEventListener node "click" $ const do
  msa <- emsa
  maybe (pure unit) (\(Changed _ a) -> callback a) msa

-- Running

-- Generic
runComponent :: forall m a. MonadBuilder m => Component m a -> a -> m Unit
runComponent c a = do
  update <- (unwrapWidget c) \(Changed scope _) -> log $ "change in scope: " <> show scope
  update (Changed Some a)

-- Web-specific
runMainComponent :: forall a. Component (Builder Unit) a -> a -> Effect Unit
runMainComponent c a = runMainBuilderInBody $ runComponent c a

-- Private

widget :: forall m i o. ((o -> Effect Unit) -> m (i -> m Unit)) -> Widget m i o
widget = Widget

unwrapWidget :: forall m i o. Widget m i o -> (o -> Effect Unit) -> m (i -> m Unit)
unwrapWidget (Widget w) = w

component :: forall a. Widget a a -> Component a
component w = widget \callback -> do
  update <- unwrapWidget w \a -> callback (Changed Some a)
  pure \(Changed scope a) -> do
    case scope of
      None -> do
        pure unit
      _ -> update a


