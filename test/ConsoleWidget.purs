module Test.ConsoleWidget
  ( ConsoleWidget(..)
  , consoleWidget
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Invariant (class AffInvariant, class CartesianInvariant, class CoCartesianInvariant, class FooInvariant, class Invariant)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref

newtype ConsoleWidget a = ConsoleWidget (a -> (a -> Effect Unit) -> Effect (a -> Effect Unit))

consoleWidget :: forall a . Show a => String -> ConsoleWidget a
consoleWidget name = ConsoleWidget \a callback -> do
    let render a = log $ "render: " <> name <> ":=" <> show a
    render a
    pure render

instance Invariant ConsoleWidget where
    invmap f g (ConsoleWidget widget) = ConsoleWidget \b callbackb -> (_ <<< g) <$> widget (g b) (callbackb <<< f)

instance CartesianInvariant ConsoleWidget where
    invfirst (ConsoleWidget widget) = ConsoleWidget \ab callbackab -> do
        bref <- Ref.new $ snd ab
        update <- widget (fst ab) \a -> do
            b <- Ref.read bref 
            callbackab (Tuple a b)
        pure  \ab -> do
            Ref.write (snd ab) bref
            -- TODO: only if we know that (fst ab) has changed 
            update (fst ab)
    invsecond (ConsoleWidget widget) = ConsoleWidget \ab callbackab -> do
        aref <- Ref.new $ fst ab
        update <- widget (snd ab) \b -> do
            a <- Ref.read aref
            callbackab (Tuple a b)
        pure  \ab -> do
            Ref.write (fst ab) aref
            -- TODO: only if we know that (snd ab) has changed
            update (snd ab)

instance CoCartesianInvariant ConsoleWidget where
    invleft (ConsoleWidget widget) = ConsoleWidget \aorb callbackaorb -> do
        -- TODO create slot
        mupdateRef <- case aorb of
            Left a -> do
                update <- widget a $ callbackaorb <<< Left
                Ref.new (Just update)
            Right b -> Ref.new Nothing
        pure \aorb -> case aorb of
            Left a -> do
              mUpdate <- Ref.read mupdateRef
              update <- case mUpdate of
                Just update -> pure update
                Nothing -> do
                    -- TODO cleanup slot
                    update <- widget a $ callbackaorb <<< Left
                    Ref.write (Just update) mupdateRef
                    pure update
              update a
            Right _ -> do
              -- TODO cleanup slot
              Ref.write Nothing mupdateRef
    invright (ConsoleWidget widget) = ConsoleWidget \aorb callbackaorb -> do
        -- TODO create slot
        mupdateRef <- case aorb of
            Right b -> do
                update <- widget b $ callbackaorb <<< Right
                Ref.new (Just update)
            Left a -> Ref.new Nothing
        pure \aorb -> case aorb of
            Right b -> do
              mUpdate <- Ref.read mupdateRef
              update <- case mUpdate of
                Just update -> pure update
                Nothing -> do
                    -- TODO cleanup slot
                    update <- widget b $ callbackaorb <<< Right
                    Ref.write (Just update) mupdateRef
                    pure update
              update b
            Left _ -> do
              -- TODO cleanup slot
              Ref.write Nothing mupdateRef

instance FooInvariant ConsoleWidget where
    invempty = ConsoleWidget $ const $ const $ pure mempty
    invappend (ConsoleWidget widget1) (ConsoleWidget widget2) = ConsoleWidget \a callbacka -> do
        -- TODO how to get rid of this ref?
        mupdate2Ref <- Ref.new Nothing
        update1 <- widget1 a $ (\a -> do
            mupdate2 <- Ref.read mupdate2Ref
            case mupdate2 of
                Just update2 -> update2 a
                Nothing -> pure unit) <> callbacka
        update2 <- widget2 a $ update1 <> callbacka
        Ref.write (Just update2) mupdate2Ref
        pure $ update1 <> update2

instance AffInvariant ConsoleWidget where
    invaff effect (ConsoleWidget widget) = ConsoleWidget \a callbacka -> do
        _ <- widget a (\a -> launchAff_ $ effect a >>= callbacka >>> liftEffect) 
        pure mempty