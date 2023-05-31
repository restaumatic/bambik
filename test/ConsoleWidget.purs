module Test.ConsoleWidget
  ( ConsoleWidget(..)
  , consoleWidget
  , constant
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Invariant (class CartesianInvariant, class CoCartesianInvariant, class EffInvariant, class FooInvariant, class ImmutableInvariant, class Invariant, class StaticInvariant)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Ref as Ref

newtype ConsoleWidget a = ConsoleWidget ((a -> Effect Unit) -> Effect (a -> Effect Unit))

consoleWidget :: String -> ConsoleWidget String
consoleWidget name = ConsoleWidget \_ -> pure \a -> log $ "render: " <> name <> ":=" <> a

constant :: forall a . Show a => a -> ConsoleWidget Void
constant a = ConsoleWidget $ \_ -> do
    log $ "static: " <> show a
    pure absurd

instance Invariant ConsoleWidget where
    invmap f g (ConsoleWidget widget) = ConsoleWidget \callbackb -> (_ <<< g) <$> widget (callbackb <<< f)

instance CartesianInvariant ConsoleWidget where
    invfirst (ConsoleWidget widget) = ConsoleWidget \callbackab -> do
        bref <- Ref.new Nothing
        update <- widget \a -> do
            mb <- Ref.read bref
            maybe (pure unit) (\b -> callbackab (Tuple a b)) mb
        pure \ab -> do
            Ref.write (Just (snd ab)) bref
            -- TODO: only if we know that (fst ab) has changed 
            update (fst ab)
    invsecond (ConsoleWidget widget) = ConsoleWidget \callbackab -> do
        aref <- Ref.new Nothing
        update <- widget \b -> do
            ma <- Ref.read aref
            maybe (pure unit) (\a -> callbackab (Tuple a b)) ma
        pure \ab -> do
            Ref.write (Just (fst ab)) aref
            -- TODO: only if we know that (snd ab) has changed
            update (snd ab)

instance CoCartesianInvariant ConsoleWidget where
    invleft (ConsoleWidget widget) = ConsoleWidget \callbackaorb -> do
        -- TODO create slot
        mupdateRef <- Ref.new Nothing
        pure \aorb -> case aorb of
            Left a -> do
              mUpdate <- Ref.read mupdateRef
              update <- case mUpdate of
                Just update -> pure update
                Nothing -> do
                    -- TODO cleanup slot
                    update <- widget $ callbackaorb <<< Left
                    Ref.write (Just update) mupdateRef
                    pure update
              update a
            Right _ -> do
              -- TODO cleanup slot
              Ref.write Nothing mupdateRef
    invright (ConsoleWidget widget) = ConsoleWidget \callbackaorb -> do
        -- TODO create slot
        mupdateRef <- Ref.new Nothing
        pure \aorb -> case aorb of
            Right b -> do
              mUpdate <- Ref.read mupdateRef
              update <- case mUpdate of
                Just update -> pure update
                Nothing -> do
                    -- TODO cleanup slot
                    update <- widget $ callbackaorb <<< Right
                    Ref.write (Just update) mupdateRef
                    pure update
              update b
            Left _ -> do
              -- TODO cleanup slot
              Ref.write Nothing mupdateRef

instance FooInvariant ConsoleWidget where
    invempty = ConsoleWidget $ const $ pure mempty
    invappend (ConsoleWidget widget1) (ConsoleWidget widget2) = ConsoleWidget \callbacka -> do
        -- TODO how to get rid of this ref?
        mupdate2Ref <- Ref.new Nothing
        update1 <- widget1 $ (\a -> do
            mupdate2 <- Ref.read mupdate2Ref
            case mupdate2 of
                Just update2 -> update2 a
                Nothing -> pure unit) <> callbacka
        update2 <- widget2 $ update1 <> callbacka
        Ref.write (Just update2) mupdate2Ref
        pure $ update1 <> update2

-- runs provided effect instead of calling callback
instance EffInvariant ConsoleWidget where
    inveff effect (ConsoleWidget widget) = ConsoleWidget \_ -> widget effect

instance StaticInvariant ConsoleWidget where
    invstatic (ConsoleWidget widget) = ConsoleWidget \_ -> do
      _ <- widget absurd
      pure mempty

instance ImmutableInvariant ConsoleWidget where
    invimmutable a (ConsoleWidget widget) = ConsoleWidget \callbacka -> do
      _ <- widget (const (callbacka a))
      pure mempty
