module Web
  ( Widget
  , aside
  , aside'
  , button
  , button'
  , checkbox
  , div
  , div'
  , element
  , element'
  , element_
  , h1
  , h1'
  , h2
  , h2'
  , h3
  , h3'
  , h4
  , h4'
  , label
  , label'
  , module Data.Profunctor.Plus
  , radioButton
  , span
  , span'
  , text
  , textInput
  )
  where

import Prelude hiding (zero)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Change (class ChProfunctor, Change(..), Changed(..))
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Plus (class ProfunctorZero, class ProfunctorPlus, proplus, proplusfirst, proplussecond, pzero, (<^), (^), (^>))
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Specular.Dom.Builder (Attrs, Builder, Node, TagName, addEventListener, appendSlot, attr, elAttr, getChecked, getValue, newSlot, populateBody, populateSlot, setAttributes, setChecked, setValue)
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
    mUpdate <- liftEffect $ populateSlot slot $ case aorb of
      Right a -> do
        update <- unwrapWidget w a (callbackchaorb <<< map Right)
        pure $ Just update
      Left _ -> pure Nothing
    mUpdateRef <- liftEffect $ Ref.new mUpdate
    case aorb of
      Left _ -> pure unit
      Right b -> void $ liftEffect $ makeUpdate slot callbackchaorb mUpdateRef b
    pure \chaorb -> case chaorb of
      Changed None _ -> pure unit
      Changed c (Right b) -> do
        mUpdate <- Ref.read mUpdateRef
        update <- case mUpdate of
          Just update -> pure update
          Nothing -> makeUpdate slot callbackchaorb mUpdateRef b
        update $ Changed c b
      _ -> do
        void $ populateSlot slot $ pure unit
        Ref.write Nothing mUpdateRef
      -- doing here instead:
      -- Left a -> do
      --   void $ populateSlot slot $ pure unit
      --   Ref.write Nothing mUpdateRef
      --   abcallback (Left a)
      -- would type check, yet it would be wrong as we don't allow a component to pass intput though to output
      -- TODO EC make it not type check
      where
        makeUpdate slot abcallback mUpdateRef a = do
          newUpdate <- populateSlot slot $ unwrapWidget w a (abcallback <<< map Right)
          Ref.write (Just newUpdate) mUpdateRef
          pure newUpdate

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

instance ChProfunctor Widget where
  chmap mapin mapout w = Widget \initial callback -> do
    update <- unwrapWidget w initial \(Changed c a) -> do
      callback $ Changed (mapout c) a
    pure \(Changed c a) -> update $ Changed (mapin c) a
  fixed a w = Widget \_ _ -> do
    void $ unwrapWidget w a mempty
    pure mempty

instance Semigroupoid Widget where
  compose w2 w1 = Widget \inita callbackc -> do
    slot <- newSlot
    liftEffect $ populateSlot slot $ unwrapWidget w1 inita \(Changed _ b) -> do
      spawnedSlot <- appendSlot slot
      void $ populateSlot spawnedSlot $ unwrapWidget w2 b callbackc
      -- note: w2 cannot be updated not destroyed externally, w2 should itself take care of its scope destroy

-- Primitives

text :: forall a. Widget String a
text = Widget \s _ -> do
  slot <- newSlot
  liftEffect $ update slot s
  pure case _ of
    Changed None _ -> pure unit
    Changed _ news -> update slot news
    where
      update slot s = populateSlot slot $ Builder.text s

textInput :: Attrs -> Widget String String -- TODO EC incorporate validation here? The id would be plain Widget?
textInput attrs = Widget \a callbackcha -> do
  Tuple node _ <- elAttr "input" attrs (pure unit)
  liftEffect $ setValue node a
  liftEffect $ addEventListener "input" node $ const $ getValue node >>= Changed Some >>> callbackcha
  pure case _ of
    Changed None _ -> mempty
    Changed _ newa -> setValue node newa

checkbox :: Attrs -> Widget Boolean Boolean
checkbox attrs = Widget \a callbackcha -> do
  Tuple node _ <- elAttr "input" (attr "type" "checkbox" <> attrs) (pure unit)
  liftEffect $ setChecked node a
  liftEffect $ addEventListener "input" node $ const $ getChecked node >>= Changed Some >>> callbackcha
  pure case _ of
    Changed None _ -> mempty
    Changed _ newa -> setChecked node newa

-- input:
-- Nothing -> turns off button
-- Just a -> turns on (if turned off) button and remembers `a`
-- output:
-- Nothing -> on button clicked when button doesn't remember any `a`
-- Just a -> on button clicked when button does remember an `a`
radioButton :: forall a. Attrs -> Widget (Maybe a) (Maybe a)
radioButton attrs = Widget \ma callbackchma -> do
  maRef <- liftEffect $ Ref.new ma
  Tuple node _ <- elAttr "input" (attr "type" "radio" <> attrs) (pure unit)
  liftEffect $ setChecked node (isJust ma)
  liftEffect $ addEventListener "change" node $ const $ Ref.read maRef >>= Changed Some >>> callbackchma
  pure case _ of
    Changed None _ -> mempty
    Changed _ Nothing -> setChecked node false
    Changed _ newma@(Just _) -> do
      Ref.write newma maRef
      setChecked node true

-- Optics

element :: forall a b. TagName -> Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
element tagName attrs dynAttrs listener w = Widget \a callbackb -> do
  aRef <- liftEffect $ Ref.new a
  Tuple node update <- elAttr tagName attrs $ unwrapWidget w a callbackb
  liftEffect $ setAttributes node (attrs <> dynAttrs a)
  liftEffect $ listener node (Ref.read aRef)
  pure case _ of
    Changed None _ -> mempty
    Changed ch newa -> do
      Ref.write newa aRef
      setAttributes node (attrs <> dynAttrs newa)
      update $ Changed ch newa

-- Element that cleans up after first output emitted, TODO EC: clean it up
element_ :: forall a b. TagName -> Attrs -> (a -> Attrs) -> (Node -> Effect a -> (b -> Effect Unit) -> Effect (Effect Unit)) -> Widget a b -> Widget a b
element_ tagName attrs dynAttrs listener w = Widget \a callbackb -> do
  aRef <- liftEffect $ Ref.new a
  cleanupRef <- liftEffect $ Ref.new Nothing
  Tuple node update <- elAttr tagName attrs $ unwrapWidget w a \chb -> do
    mCleanup <- Ref.read cleanupRef
    callbackb chb
    fromMaybe mempty mCleanup
  liftEffect $ setAttributes node (attrs <> dynAttrs a)
  cleanup <- liftEffect $ listener node (Ref.read aRef) (callbackb <<< Changed Some)
  liftEffect $ Ref.write (Just cleanup) cleanupRef
  pure case _ of
    Changed None _ -> mempty
    Changed ch newa -> do
      Ref.write newa aRef
      setAttributes node (attrs <> dynAttrs newa)
      update $ Changed ch newa

element' :: forall a b. TagName -> Widget a b -> Widget a b
element' tagName = element tagName mempty mempty mempty

div' :: forall a b. Widget a b -> Widget a b
div' = element' "div"

div :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
div = element "div"

span' :: forall a b. Widget a b -> Widget a b
span' = element' "span"

span :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
span = element "span"

aside' :: forall a b. Widget a b -> Widget a b
aside' = element' "aside"

aside :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> (b -> Effect Unit) -> Effect (Effect Unit)) -> Widget a b -> Widget a b
aside = element_ "aside"

label' :: forall a b. Widget a b -> Widget a b
label' = element' "label"

label :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
label = element "label"

button :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> (b -> Effect Unit) -> Effect (Effect Unit)) -> Widget a b -> Widget a b
button = element_ "button"

button' :: forall a b. Widget a b -> Widget a b
button' = element' "button"

h1 :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
h1 = element "h1"

type Component m a = Widget m (Changed a) (Changed a)

h1' :: forall a b. Widget a b -> Widget a b
h1' = element' "h1"

h2 :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
h2 = element "h2"

h2' :: forall a b. Widget a b -> Widget a b
h2' = element' "h2"

h3 :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
h3 = element "h3"

h3' :: forall a b. Widget a b -> Widget a b
h3' = element' "h3"

h4 :: forall a b. Attrs -> (a -> Attrs) -> (Node -> Effect a -> Effect Unit) -> Widget a b -> Widget a b
h4 = element "h4"

h4' :: forall a b. Widget a b -> Widget a b
h4' = element' "h4"

-- Entry point

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
