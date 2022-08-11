module Test.Main where

import Prelude

import Always (Always, always, unalways, yes, yesn't)
import Always.Hyrule (alwaysEvent, unalwaysEvent)
import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global, toEffect)
import Control.Monad.ST.Ref as STRef
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Event (AnEvent, bang, subscribe)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Always" do
    it "nullifies an Effect" $ liftEffect do
      condition <- Ref.new false
      _ <- toEffect $ unalways yesn't $ always do
        void $ Ref.write true condition
      result <- Ref.read condition
      result `shouldEqual` false
    it "restores an Effect" $ liftEffect do
      condition <- Ref.new false
      _ <- unalways yes $ always do
        void $ Ref.write true condition
      result <- Ref.read condition
      result `shouldEqual` true
  describe "Always.Hyrule" do
    it "nullifies an Effect" $ liftEffect do
      condition <- toEffect $ STRef.new false
      let
        alEvent :: AnEvent (Always Effect) Unit
        alEvent = alwaysEvent $ bang unit

        alEvent' :: AnEvent (ST Global) Unit
        alEvent' = unalwaysEvent yesn't alEvent
      _ <- toEffect $ subscribe alEvent' \_ -> do
             void $ STRef.write true condition
      result <- toEffect $ STRef.read condition
      result `shouldEqual` false
    it "restores an Effect" $ liftEffect do
      condition <- Ref.new false
      let
        alEvent :: AnEvent (Always Effect) Unit
        alEvent = alwaysEvent $ bang unit

        alEvent' :: AnEvent Effect Unit
        alEvent' = unalwaysEvent yes alEvent
      _ <- subscribe alEvent' \_ -> do
             void $ Ref.write true condition
      result <- Ref.read condition
      result `shouldEqual` true
