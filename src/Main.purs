module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as Array
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Node.FS (FS)
import Node.FS.Stats (isDirectory)
import Node.FS.Sync (readdir, stat)
import Node.Path (extname)
import Node.Path as Path
import Prelude (Unit, bind, compare, compose, eq, flip, map, pure, show)

getPrevFile
  :: forall e
  . String
  -> Eff
    (exception :: EXCEPTION, fs :: FS | e)
    (Maybe String)
getPrevFile dir = do
  files <- map (map (compose Path.concat (Array.snoc [dir]))) (readdir dir)
  let
    filter = Array.filter (compose (eq ".json") extname)
    sortedFiles = Array.sortBy (flip compare) files -- order by desc
    filteredFiles = filter sortedFiles
    lastFilteredFile = Array.head filteredFiles
  case lastFilteredFile of
    Just f -> pure (Just f)
    Nothing -> do
      maybes <- for sortedFiles \f -> do
        s <- stat f
        pure if isDirectory s
          then Just f
          else Nothing
      dirs <- pure (Array.catMaybes maybes)
      let
        f Nothing b = getPrevFile b
        f m@(Just a) _ = pure m
      foldM f Nothing dirs

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
  let dir = "./src"
  file <- getPrevFile dir
  log (show file)
