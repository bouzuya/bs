module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as Array
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Ord (greaterThan)
import Data.String as String
import Data.Traversable (for)
import Node.FS (FS)
import Node.FS.Stats (isDirectory)
import Node.FS.Sync (readdir, stat)
import Node.Path (FilePath, dirname, extname, resolve)
import Node.Path as Path
import Prelude (Unit, bind, compare, compose, const, eq, flip, map, otherwise, pure, show)

readDir'
  :: forall e
  . FilePath -> Eff (exception :: EXCEPTION, fs :: FS | e) (Array FilePath)
readDir' dir = map (map (compose Path.concat (Array.snoc [dir]))) (readdir dir)

startsWith :: String -> String -> Boolean
startsWith p = compose (eq (Just 0)) (String.indexOf (String.Pattern p))

getPrevFile
  :: forall e
  . FilePath
  -> Maybe FilePath
  -> Eff
    (exception :: EXCEPTION, fs :: FS | e)
    (Maybe FilePath)
getPrevFile root file =
  go root (maybe root dirname file) file
  where
    go root dir file | startsWith root dir = do
      files <- readDir' dir
      let
        prevFilter = maybe (const true) greaterThan file
        files' = Array.filter prevFilter files
        sortedFiles = Array.sortBy (flip compare) files' -- order by desc
      case Array.find (compose (eq ".json") extname) sortedFiles of
        Just f -> pure (Just f)
        Nothing -> do
          -- search child dir
          maybes <- for sortedFiles \f -> do
            s <- stat f
            pure (guard (isDirectory s) (Just f))
          let
            dirs = Array.catMaybes maybes
            g Nothing d = go root d Nothing
            g m@(Just a) _ = pure m
          m <- foldM g Nothing dirs
          -- TODO: search parent dir
          case m of
            Just f -> pure (Just f)
            Nothing -> pure Nothing
    go _ _ _ | otherwise = pure Nothing

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
    root = resolve [] "." -- ? No Effect ?
    cur = resolve [] "./package-lock.json" -- ? No Effect ?
  file <- getPrevFile root (Just cur)
  log (show file)
