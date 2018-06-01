module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as Array
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..))
import Data.Ord (greaterThan)
import Data.String as String
import Data.Traversable (for)
import Node.FS (FS)
import Node.FS.Stats (isDirectory)
import Node.FS.Sync (readdir, stat)
import Node.Path (FilePath, dirname, extname, resolve)
import Node.Path as Path
import Prelude (Unit, bind, compare, compose, conj, const, eq, flip, map, pure, show)

readDir'
  :: forall e
  . FilePath -> Eff (exception :: EXCEPTION, fs :: FS | e) (Array FilePath)
readDir' dir = map (map (compose Path.concat (Array.snoc [dir]))) (readdir dir)

startsWith :: String -> String -> Boolean
startsWith p = compose (eq (Just 0)) (String.indexOf (String.Pattern p))

getPrevFile
  :: forall e
  . FilePath
  -> FilePath
  -> Eff
    (exception :: EXCEPTION, fs :: FS | e)
    (Maybe FilePath)
getPrevFile root file = do
  fileStat <- stat file
  let dir = if isDirectory fileStat then file else dirname file
  files <- readDir' dir
  let
    rootFilter = startsWith root
    prevFilter = if isDirectory fileStat
      then const true
      else greaterThan file
    files' = Array.filter (\f -> conj (prevFilter f) (rootFilter f)) files
    sortedFiles = Array.sortBy (flip compare) files' -- order by desc
    filter = Array.filter (compose (eq ".json") extname)
    sortAndFilteredFiles = filter sortedFiles
  case Array.head sortAndFilteredFiles of
    Just f -> pure (Just f)
    Nothing -> do
      maybes <- for sortedFiles \f -> do
        s <- stat f
        pure if isDirectory s
          then Just f
          else Nothing
      let
        dirs = Array.catMaybes maybes
        f Nothing b = getPrevFile root b
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
  let
    dir = resolve [] "." -- ? No Effect ?
    cur = resolve [] "./package-lock.json" -- ? No Effect ?
  file <- getPrevFile dir cur
  log (show file)
