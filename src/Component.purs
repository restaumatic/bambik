module Component
  ( Component(..)
  , ComponentWrapper
  , checkbox
  , inside
  , noChoiceComponent
  , onClick
  , radio
  , renderComponent
  -- , static
  , staticText
  , swallow
  , swallow'
  , text
  , textInput
  )
  where

import Prelude hiding (zero)

import Control.Monad.Replace (destroySlot, newSlot, replaceSlot)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Invariant (class CartesianInvariant, class CoCartesianInvariant, class Invariant)
import Data.Invariant.Cayley (CayleyInvariant)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, modify, unwrap, wrap)
import Data.Plus (class Plus, zero)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Specular.Dom.Browser (Attrs, Node, TagName, setAttributes, (:=))
import Specular.Dom.Browser as DOM
import Specular.Dom.Builder (Builder)
import Specular.Dom.Builder.Class (elDynAttr')
import Specular.Dom.Builder.Class as S
import Specular.Dom.Widgets.Input (setCheckboxChecked, setTextInputValue)
import Specular.FRP (weaken)

newtype Component :: Type -> Type
newtype Component a = Component ((a -> Effect Unit) -> Builder Unit (a -> Effect Unit))

derive instance Newtype (Component a) _

static' :: forall a . Component Void -> Component a
static' (Component widget) = Component \_ -> do
    _ <- widget absurd
    pure mempty

sta :: forall a s . a -> Component a -> Component s
sta a c = wrap \callback -> do
  update <- unwrap c mempty
  liftEffect $ update a
  pure mempty


instance Plus Component where
  plus c1 c2 = wrap \callback -> do
    -- TODO how to get rid of this ref?
    mUpdate2Ref <- liftEffect $ Ref.new Nothing
    update1 <- unwrap c1 $ (\a -> do
      mUpdate2 <- Ref.read mUpdate2Ref
      case mUpdate2 of
        Just update2 -> update2 a
        Nothing -> pure unit) <> callback
    update2 <- unwrap c2 $ update1 <> callback
    pure \i -> do
      update1 i
      update2 i
  zero = Component mempty

instance Invariant Component where
  invmap pre post c = wrap \callback -> do
      f <- unwrap c $ callback <<< pre
      pure $ f <<< post

instance CartesianInvariant Component where
  invfirst c = wrap \abcallback -> do
    bref <- liftEffect $ Ref.new Nothing
    update <- unwrap c \a -> do
      mb <- liftEffect $ Ref.read bref
      maybe (pure unit) (\b -> abcallback (Tuple a b)) mb
    pure $ \ab -> do
      Ref.write (Just (snd ab)) bref
      update (fst ab)
  invsecond c = wrap \abcallback -> do
    aref <- liftEffect $ Ref.new Nothing
    update <- unwrap c \b -> do
      ma <- liftEffect $ Ref.read aref
      maybe (pure unit) (\a -> abcallback (Tuple a b)) ma
    pure $ \ab -> do
      Ref.write (Just (fst ab)) aref
      update (snd ab)

instance CoCartesianInvariant Component where
  invleft c = wrap \abcallback -> do
    slot <- newSlot
    mUpdateRef <- liftEffect $ Ref.new Nothing
    pure case _ of
        Left a -> do
          mUpdate <- liftEffect $ Ref.read mUpdateRef
          update <- case mUpdate of
            Just update -> pure update
            Nothing -> do
              newUpdate <- liftEffect $ replaceSlot slot $ unwrap c (abcallback <<< Left)
              liftEffect $ Ref.write (Just newUpdate) mUpdateRef
              pure newUpdate
          update a
        Right _ -> do
          liftEffect $ destroySlot slot
          pure unit
  invright c = wrap \abcallback -> do
    slot <- newSlot
    mUpdateRef <- liftEffect $ Ref.new Nothing
    pure case _ of
        Right b -> do
          mUpdate <- liftEffect $ Ref.read mUpdateRef
          update <- case mUpdate of
            Just update -> pure update
            Nothing -> do
              newUpdate <- liftEffect $ replaceSlot slot $ unwrap c (abcallback <<< Right)
              liftEffect $ Ref.write (Just newUpdate) mUpdateRef
              pure newUpdate
          update b
        Left _ -> do
          liftEffect $ destroySlot slot
          pure unit


noChoiceComponent :: forall a. Component a
noChoiceComponent = wrap $ pure mempty

renderComponent :: forall a. ComponentWrapper Identity a -> (a -> Effect Unit) -> Builder Unit (a -> Effect Unit)
renderComponent componentw callback = do
  let (Identity component) = unwrap componentw
  unwrap component callback

-- Component primitives

staticText :: forall f a . Applicative f => String -> ComponentWrapper f a
staticText content = wrap $ pure $ wrap $ const $ S.text content *> mempty

text :: forall f . Applicative f => ComponentWrapper f String
text = wrap $ pure $ wrap \_ -> do
  slot <- newSlot
  pure \t -> do
    replaceSlot slot $ S.text t

inside :: forall f a b. Functor f => TagName -> (a -> Attrs) -> (a -> Node -> (b -> Effect Unit) -> Effect Unit) -> ComponentWrapper f a -> ComponentWrapper f a
inside tagName attrs event = modify $ map \component -> wrap \callback -> do
  Tuple node f <- elDynAttr' tagName (weaken (pure mempty)) $ unwrap component callback -- TODO: stop using (Weak)Dynamic
  pure \a -> do
    f a
    setAttributes node (attrs a)
    -- TODO: propagate events from wrapper?
    -- event a node bcallback
  -- outerEvent <- event dyn node
  -- pure $ innerEvent <> outerEvent

textInput :: forall f. Applicative f => (String -> Attrs) -> ComponentWrapper f String
textInput attrs = zero # inside "input" attrs \str node callback -> do
  setTextInputValue node str
  -- (domEventWithSample (\_ -> getTextInputValue node <#> \value -> {path: [], value}) "input" node)

checkbox :: forall f. Applicative f => (Boolean -> Attrs) -> ComponentWrapper f Boolean
checkbox attrs = zero # inside "input" (\enabled -> ("type" := "checkbox") <> (if enabled then "checked" := "checked" else mempty) <> attrs enabled) \bool node callback -> do
  setCheckboxChecked node bool
  -- domEventWithSample (\_ -> getCheckboxChecked node <#> \value -> { path: [], value }) "change" node

radio :: forall f. Applicative f => (Boolean -> Attrs) -> ComponentWrapper f Boolean
radio attrs = zero # inside "input" (\enabled -> ("type" := "radio") <> (if enabled then "checked" := "checked" else mempty) <> attrs enabled) \value node callback -> do
  setCheckboxChecked node value
  -- onDomEvent "change" node (\_ -> getCheckboxChecked node >>= callback)
-- radio :: forall a b f. Applicative f => (a -> Attrs) -> ComponentWrapper f a b
-- radio attrs = zero # (inside "input" (\enabled -> ("type" := "checkbox") <> (if enabled then "checked" := "checked" else mempty) <> attrs enabled) \_ node -> do
--   domEventWithSample (\_ -> getCheckboxChecked node <#> \value -> { path: [], value }) "change" node)


onClick ∷ forall a. a → Node → (a -> Effect Unit) -> Effect Unit
onClick a node callback = void $ DOM.addEventListener "click" (\_ -> callback a) node

-- Foldables

swallow :: forall f a . Functor f => ComponentWrapper f a -> ComponentWrapper f a
swallow = unwrap >>> map (\component -> wrap \callback -> do
  f <- unwrap component mempty
  pure $ \a -> f a) >>> wrap

swallow' :: forall a. Component a
swallow' = wrap $ mempty

static :: forall f a b. Functor f => a -> ComponentWrapper f a -> ComponentWrapper f b
static a = unwrap >>> map (\component -> wrap \callback -> do
  f <- unwrap component mempty
  pure \_ -> f a) >>> wrap

--

type ComponentWrapper f a = CayleyInvariant f Component a

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

type Path = Array Hop

data Hop = HopLeft | HopRight | HopFirst | HopSecond

derive instance Generic Hop _
derive instance Eq Hop

instance Show Hop where
  show = genericShow

-- Dynamic for lenses
type WithPath a =
  { path :: Path
  , value :: a
  }
