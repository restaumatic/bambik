module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Invariant (class CartesianInvariant, class CoCartesianInvariant, class FooInvariant, class Invariant, invappend)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Ref as Ref

newtype Widget a = Widget (a -> (a -> Effect Unit) -> Effect (a -> Effect Unit))

widget :: forall a . Show a => String -> Widget a
widget name = Widget \a callback -> do
    let render a = log $ "render: " <> name <> ":=" <> show a
    render a
    pure render

instance Invariant Widget where
    invmap f g (Widget widget) = Widget  \b callbackb -> (_ <<< g) <$> widget (g b) (callbackb <<< f)

instance CartesianInvariant Widget where
    invfirst (Widget widget) = Widget \ab callbackab -> do
        bref <- Ref.new $ snd ab
        update <- widget (fst ab) \a -> do
            b <- Ref.read bref 
            callbackab (Tuple a b)
        pure  \ab -> do
            Ref.write (snd ab) bref
            -- TODO: only if we know that (fst ab) has changed 
            update (fst ab)
    invsecond (Widget widget) = Widget \ab callbackab -> do
        aref <- Ref.new $ fst ab
        update <- widget (snd ab) \b -> do
            a <- Ref.read aref
            callbackab (Tuple a b)
        pure  \ab -> do
            Ref.write (fst ab) aref
            -- TODO: only if we know that (snd ab) has changed
            update (snd ab)

instance CoCartesianInvariant Widget where
    invleft (Widget widget) = Widget \aorb callbackaorb -> do
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
    invright (Widget widget) = Widget \aorb callbackaorb -> do
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

instance FooInvariant Widget where
    invempty = Widget $ const $ const $ pure mempty
    invappend (Widget widget1) (Widget widget2) = Widget \a callbacka -> do
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

-- Invariant transformers

-- References
-- 1. Edward Kmett: Rotten Bananas, http://comonad.com/reader/2008/rotten-bananas/  


main :: Effect Unit
main = do
  let (Widget w) = widget "widget1" `invappend` widget "widget2"
  _ <- w "value" mempty
  pure unit
