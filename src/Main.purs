module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as Array
import Node.FS (FS)
import Node.FS.Sync (readdir)
import Prelude (Unit, bind, discard, show, (<$>), (<<<))

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
  file <- Array.last <<< Array.sort <$> readdir currentDirectory
  log (show file)
  log "Hello sailor!"
