module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe(..))
import Main (getPrevFile)
import Node.FS (FS)
import Node.Path (resolve)
import Prelude (Unit, bind, discard, ($), (<>))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main
  :: forall e
  . Eff
    ( avar :: AVAR
    , console :: CONSOLE
    , exception :: EXCEPTION
    , fs :: FS
    , testOutput :: TESTOUTPUT
    | e
    )
    Unit
main = do
  runTest do
    test "1" do
      let dir = resolve [] "test/data/1"
      r <- liftEff $ getPrevFile dir Nothing
      Assert.equal r Nothing
    suite "2" do
      let dir = resolve [] "test/data/2"
      test "1" do
        r <- liftEff $ getPrevFile dir Nothing
        Assert.equal r (Just (dir <> "/1.json"))
      test "2" do
        r <- liftEff $ getPrevFile dir (Just (dir <> "/1.json"))
        Assert.equal r Nothing
    suite "3" do
      let dir = resolve [] "test/data/3"
      test "1" do
        r <- liftEff $ getPrevFile dir Nothing
        Assert.equal r (Just (dir <> "/2.json"))
      test "2" do
        r <- liftEff $ getPrevFile dir (Just (dir <> "/2.json"))
        Assert.equal r (Just (dir <> "/1.json"))
      test "3" do
        r <- liftEff $ getPrevFile dir (Just (dir <> "/1.json"))
        Assert.equal r Nothing
    suite "4" do
      let dir = resolve [] "test/data/4"
      test "1" do
        r <- liftEff $ getPrevFile dir Nothing
        Assert.equal r (Just (dir <> "/2/1.json"))
      test "2" do
        r <- liftEff $ getPrevFile dir (Just (dir <> "/2/1.json"))
        Assert.equal r (Just (dir <> "/1/1.json"))
      test "3" do
        r <- liftEff $ getPrevFile dir (Just (dir <> "/1/1.json"))
        Assert.equal r Nothing
    suite "5" do
      let dir = resolve [] "test/data/5"
      test "1" do
        r <- liftEff $ getPrevFile dir Nothing
        Assert.equal r (Just (dir <> "/2/1.json"))
