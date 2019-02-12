module Test.Main where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Lib (getPrevFile')
import Node.Path (resolve)
import Prelude (Unit, bind, discard, pure, ($), (<>))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  runTest do
    test "1" do
      let getDirectory = resolve [] "test/data/1"
      d <- liftEffect getDirectory
      actual <- liftEffect $ getPrevFile' d Nothing
      expected <- pure Nothing
      Assert.equal expected actual
    suite "2" do
      let getDirectory = resolve [] "test/data/2"
      test "1" do
        d <- liftEffect getDirectory
        actual <- liftEffect $ getPrevFile' d Nothing
        expected <- pure $ Just $ d <> "/1.json"
        Assert.equal expected actual
      test "2" do
        d <- liftEffect getDirectory
        actual <- liftEffect $ getPrevFile' d $ Just $ d <> "/1.json"
        expected <- pure Nothing
        Assert.equal expected actual
    suite "3" do
      let getDirectory = resolve [] "test/data/3"
      test "1" do
        d <- liftEffect getDirectory
        actual <- liftEffect $ getPrevFile' d Nothing
        expected <-pure $ Just $ d <> "/2.json"
        Assert.equal expected actual
      test "2" do
        d <- liftEffect getDirectory
        actual <- liftEffect $ getPrevFile' d $ Just $ d <> "/2.json"
        expected <- pure $ Just $ d <> "/1.json"
        Assert.equal expected actual
      test "3" do
        d <- liftEffect getDirectory
        actual <- liftEffect $ getPrevFile' d $ Just $ d <> "/1.json"
        expected <- pure Nothing
        Assert.equal expected actual
    suite "4" do
      let getDirectory = resolve [] "test/data/4"
      test "1" do
        d <- liftEffect getDirectory
        actual <- liftEffect $ getPrevFile' d Nothing
        expected <- pure $ Just $ d <> "/2/1.json"
        Assert.equal expected actual
      test "2" do
        d <- liftEffect getDirectory
        actual <- liftEffect $ getPrevFile' d $ Just $ d <> "/2/1.json"
        expected <- pure $ Just $ d <> "/1/1.json"
        Assert.equal expected actual
      test "3" do
        d <- liftEffect getDirectory
        actual <- liftEffect $ getPrevFile' d $ Just $ d <> "/1/1.json"
        expected <- pure Nothing
        Assert.equal expected actual
    suite "5" do
      let getDirectory = resolve [] "test/data/5"
      test "1" do
        d <- liftEffect getDirectory
        actual <- liftEffect $ getPrevFile' d Nothing
        expected <- pure $ Just $ d <> "/2/1.json"
        Assert.equal expected actual
      test "2" do
        d <- liftEffect getDirectory
        actual <- liftEffect $ getPrevFile' d $ Just $ d <> "/2/1.json"
        expected <- pure $ Just $ d <> "/1.json"
        Assert.equal expected actual
      test "3" do
        d <- liftEffect getDirectory
        actual <- liftEffect $ getPrevFile' d $ Just $ d <> "/1.json"
        expected <- pure Nothing
        Assert.equal expected actual
