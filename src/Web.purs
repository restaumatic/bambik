module Web
  ( Component(..)
  , Tag
  , Hop
  , checkbox
  , inside
  , noChoiceComponent
  , onClick
  , radio
  , buildComponent
  -- , static
  , staticText
  , text
  , textInput
  )
  where

import Prelude hiding (zero)

import Control.Monad.Replace (destroySlot, newSlot, replaceSlot)
import Data.Either (Either(..))
import Data.Invariant (class Cartesian, class CoCartesian, class Invariant, class Tagged)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Plus (class Plus, zero)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Specular.Dom.Browser (Attrs, Node, TagName, onDomEvent, (:=))
import Specular.Dom.Builder (Builder)
import Specular.Dom.Builder.Class (elAttr)
import Specular.Dom.Builder.Class as S

newtype Component :: Type -> Type
newtype Component a = Component
  { builder :: (a -> Effect Unit) -> Builder Unit (a -> Effect Unit)
  , tag :: Tag
  }

derive instance Newtype (Component a) _

withoutTag :: forall a . ((a -> Effect Unit) -> Builder Unit (a -> Effect Unit)) -> Component a
withoutTag builder = Component { builder, tag: mempty}

-- static' :: forall a . Component Void -> Component a
-- static' (Component widget) = Component \_ -> do
--     _ <- widget absurd
--     pure mempty

-- sta :: forall a s . a -> Component a -> Component s
-- sta a c = wrap \callback -> do
--   update <- unwrap c mempty
--   liftEffect $ update a
--   pure mempty


type Hop = String

type Tag = Array (Array Hop)

instance Tagged Tag Component where
  getTag component = (unwrap component).tag
  setTag tag (Component { builder } ) = Component {builder, tag}


instance Plus Component where
  plus c1 c2 = wrap
    { builder: \callback -> do
      -- TODO how to get rid of this ref?
      mUpdate2Ref <- liftEffect $ Ref.new Nothing
      update1 <- (unwrap c1).builder $ (\a -> do
        mUpdate2 <- Ref.read mUpdate2Ref
        case mUpdate2 of
          Just update2 -> update2 a
          Nothing -> pure unit) <> callback
      update2 <- (unwrap c2).builder $ update1 <> callback
      liftEffect $ Ref.write (Just update2) mUpdate2Ref
      pure \i -> do
        update1 i
        update2 i
    , tag: (unwrap c1).tag <> (unwrap c2).tag
    }
  zero = Component
    { builder: mempty
    , tag: mempty
    }

instance Invariant Component where
  invmap pre post c = wrap
    { builder: \callback -> do
      f <- (unwrap c).builder $ callback <<< pre
      pure $ f <<< post
    , tag: (unwrap c).tag
    }

instance Cartesian Component where
  invfirst c = wrap
    { builder: \abcallback -> do
      bref <- liftEffect $ Ref.new Nothing
      update <- (unwrap c).builder \a -> do
        mb <- liftEffect $ Ref.read bref
        maybe (pure unit) (\b -> abcallback (Tuple a b)) mb
      pure $ \ab -> do
        Ref.write (Just (snd ab)) bref
        update (fst ab)
    , tag: (unwrap c).tag
    }
  invsecond c = wrap
    { builder: \abcallback -> do
      aref <- liftEffect $ Ref.new Nothing
      update <- (unwrap c).builder \b -> do
        ma <- liftEffect $ Ref.read aref
        maybe (pure unit) (\a -> abcallback (Tuple a b)) ma
      pure $ \ab -> do
        Ref.write (Just (fst ab)) aref
        update (snd ab)
    , tag: (unwrap c).tag
    }

instance CoCartesian Component where
  invleft c = wrap
    { builder: \abcallback -> do
      slot <- newSlot
      mUpdateRef <- liftEffect $ Ref.new Nothing
      pure case _ of
          Left a -> do
            mUpdate <- liftEffect $ Ref.read mUpdateRef
            update <- case mUpdate of
              Just update -> pure update
              Nothing -> do
                newUpdate <- liftEffect $ replaceSlot slot $ (unwrap c).builder (abcallback <<< Left)
                liftEffect $ Ref.write (Just newUpdate) mUpdateRef
                pure newUpdate
            update a
          Right _ -> do
            liftEffect $ destroySlot slot
            pure unit
    , tag: (unwrap c).tag
    }
  invright c = wrap
    { builder: \abcallback -> do
      slot <- newSlot
      mUpdateRef <- liftEffect $ Ref.new Nothing
      pure case _ of
          Right b -> do
            mUpdate <- liftEffect $ Ref.read mUpdateRef
            update <- case mUpdate of
              Just update -> pure update
              Nothing -> do
                newUpdate <- liftEffect $ replaceSlot slot $ (unwrap c).builder (abcallback <<< Right)
                liftEffect $ Ref.write (Just newUpdate) mUpdateRef
                pure newUpdate
            update b
          Left _ -> do
            liftEffect $ destroySlot slot
            pure unit
    , tag: (unwrap c).tag
    }


noChoiceComponent :: forall a. Component a
noChoiceComponent = withoutTag $ pure mempty

-- TODO: buildComponent?
buildComponent :: forall a. Component a -> (a -> Effect Unit) -> Builder Unit (a -> Effect Unit)
buildComponent component = (unwrap component).builder

-- Component primitives

staticText :: forall a . String -> Component a
staticText content = withoutTag $ const $ S.text content *> mempty

text :: Component String
text = withoutTag \_ -> do
  slot <- newSlot
  pure $ replaceSlot slot <<< S.text

inside :: forall a . TagName -> (Unit -> Attrs) -> (Node -> (a -> Effect Unit) -> Effect Unit) -> Component a -> Component a
inside tagName attrs event c = wrap
  { builder: \callback -> do
    Tuple node f <- elAttr tagName (attrs unit) $ (unwrap c).builder callback
    liftEffect $ event node callback
    pure \a -> do
      f a
  , tag: (unwrap c).tag
  }

foreign import getTextInputValue :: Node -> Effect String
foreign import setTextInputValue :: Node -> String -> Effect Unit

foreign import getCheckboxChecked :: Node -> Effect Boolean
foreign import setCheckboxChecked :: Node -> Boolean -> Effect Unit

textInput :: Attrs -> Component String
textInput attrs = withoutTag \callback -> do
  Tuple node a <- elAttr "input" attrs (pure unit)
  onDomEvent "input" node \event -> do
    getTextInputValue node >>= callback
  pure $ setTextInputValue node

checkbox :: Attrs -> Component Boolean
checkbox attrs = withoutTag \callback -> do
  Tuple node a <- elAttr "input" attrs (pure unit)
  onDomEvent "input" node \event -> do
    getCheckboxChecked node >>= callback
  pure $ setCheckboxChecked node

-- TODO
radio :: (Boolean -> Attrs) -> Component Boolean
radio attrs = zero # inside "input" (\_ -> let enabled = false in ("type" := "radio") <> (if enabled then "checked" := "checked" else mempty) <> attrs enabled) \node callback -> do
  mempty
  -- setCheckboxChecked node value
  -- onDomEvent "change" node (\_ -> getCheckboxChecked node >>= callback)
-- radio :: forall a b f. Applicative f => (a -> Attrs) -> ComponentWrapper f a b
-- radio attrs = zero # (inside "input" (\enabled -> ("type" := "checkbox") <> (if enabled then "checked" := "checked" else mempty) <> attrs enabled) \_ node -> do
--   domEventWithSample (\_ -> getCheckboxChecked node <#> \value -> { path: [], value }) "change" node)


-- TODO
onClick ∷ forall a. Node → (a -> Effect Unit) -> Effect Unit
onClick node callback = mempty -- void $ DOM.addEventListener "click" (\_ -> callback a) node

-- Foldables

-- swallow :: forall f a . Functor f => ComponentWrapper f a -> ComponentWrapper f a
-- swallow = unwrap >>> map (\component -> wrap \callback -> do
--   f <- unwrap component mempty
--   pure $ \a -> f a) >>> wrap

-- swallow' :: forall a. Component a
-- swallow' = wrap $ mempty

-- static :: forall f a b. Functor f => a -> ComponentWrapper f a -> ComponentWrapper f b
-- static a = unwrap >>> map (\component -> wrap \callback -> do
--   f <- unwrap component mempty
--   pure \_ -> f a) >>> wrap

--

-- when :: forall a p. Profunctor p => Choice p => String -> a -> p a b -> p s t
-- when  


-- when :: forall a p. Profunctor p => Choice p => String -> a -> p a b -> p s t 
-- when functionName functionValue = 

-- whenCast :: forall p s. Profunctor p => Strong p => Choice p => p s Boolean -> p s s -> p s s
-- -- whenCast cast p = p # rmap (\s -> Tuple s s) # first cast # rmap (\s checked -> if checked then Right s else Left unit) # right identity
-- whenCast cast p =

-- invariantFailedMessage :: Cast s String 
-- invariantFailedMessage :: forall p. Profunctor p => Strong p => Choice p=> VirtualField p s Boolean = prism

--

-- variadic function to append ComponentWrappers
-- component' ::  forall f a b cws. ComponentWrappers f a b cws => (ComponentWrapper f a b -> ComponentWrapper f a b) -> cws
-- component' optics = cappend optics []

-- component = component' identity
-- inside' t as es = component' (inside t as es)


-- class ComponentWrappers f a cws | cws -> f a where
--   cappend :: (ComponentWrapper f a -> ComponentWrapper f a) -> Array (ComponentWrapper f a) -> cws

-- instance Applicative f => ComponentWrappers f a (ComponentWrapper f a) where
--   cappend optics cws = fold cws

-- instance ComponentWrappers f a b r => ComponentWrappers f a b (ComponentWrapper f a b -> r) where
--   cappend optics cws cw = cappend identity (cws <> [cw])


-- inside' ::  forall p a b cws. ComponentWrappers p a b cws => TagName -> (a -> Attrs) -> (Dynamic (WithPath a) -> Node -> Widget (Event (WithPath b))) -> cws
-- inside' tag attrs events cws = inside tag attrs events $ component cws
-- inside' ∷ ∀ (b1714 ∷ Type) (f1717 ∷ Type -> Type) (a1718 ∷ Type) (b1719 ∷ Type) (t1720 ∷ Type -> Type) (t1721 ∷ Type) (t1722 ∷ Type). Functor f1717 ⇒ ComponentWrappers t1720 t1721 t1722 (b1714 -> Cayley f1717 Component a1718 b1719) ⇒ String → (a1718 → Object String) → (Dynamic { path ∷ Array Hop , value ∷ a1718 } → Node → Builder Unit (Event { path ∷ Array Hop , value ∷ b1719 } ) ) → b1714 → Cayley f1717 Component a1718 b1719
-- inside' :: forall a b. TagName -> (a -> Attrs) -> (Dynamic (WithPath a) -> Node -> Widget (Event (WithPath b))) -> Unit
-- inside' tag attrs events = component (inside tag attrs events)

-- (ComponentWrapper f a b -> ComponentWrapper f a b) instantiates ComponentWrappers

-- instance ComponentWrappers f a b (ComponentWrapper f a b -> ComponentWrapper f a b)

-- foo :: forall f a b. Applicative f => Array (ComponentWrapper f a b) -> (ComponentWrapper f a b -> ComponentWrapper f a b)
-- foo cws = cappend cws



-- prod :: forall a b s. Lens' s a -> Lens' s a -> Lens' s (Tuple a b) 
-- prod o1 o2 p = let 
--   Shop s2a sa2s = o1 (Shop identity fst)
--   Shop s2b sb2s = o2 (Shop identity fst)
--   in Shop (\s -> Tuple (s2a s) (s2b s)) (\s (Tuple a b) -> sb2s (sa2s s a) b)

-- dimap (fork v id) u · first
