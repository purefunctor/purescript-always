module Test.Main where

import Prelude

import Always (always, unalways, yesn't)
import Always.Hyrule (alwaysEventYesn't, unalwaysEventYesn't)
import Control.Monad.ST (ST)
import Control.Monad.ST.Global (toEffect)
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
  describe "Always.Hyrule" do
    it "nullifies an Event" $ liftEffect do
      condition <- Ref.new false
      let (stEvent :: AnEvent (_ (ST _)) _) = alwaysEventYesn't (bang unit :: AnEvent Effect Unit)
      _ <- subscribe (unalwaysEventYesn't stEvent) \_ ->
             void $ Ref.write true condition
      result <- Ref.read condition
      result `shouldEqual` false
