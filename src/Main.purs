module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as Array
import Node.FS (FS)
import Node.FS.Sync (readdir)
import Node.Path (extname)
import Prelude (Unit, bind, compose, discard, eq, map, show, (<<<))

main
  :: forall e
  . Eff
    ( console :: CONSOLE
    , exception :: EXCEPTION
    , fs :: FS
    | e
    )
    Unit
main = do
  let
    currentDirectory = "."
  files <- readdir currentDirectory
  let
    filter = Array.filter (compose (eq ".json") extname)
    filteredFiles = filter files
    lastFilteredFile = compose Array.last Array.sort filteredFiles
  log (show lastFilteredFile)
  log "Hello sailor!"
