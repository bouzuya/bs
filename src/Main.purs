module Main
  ( getPrevFile
  , main
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as Array
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (append, guard)
import Data.Ord (greaterThan)
import Data.String as String
import Data.Traversable (for)
import Node.FS (FS)
import Node.FS.Stats (isDirectory)
import Node.FS.Sync (readdir, stat)
import Node.Path (FilePath, dirname, extname, resolve)
import Node.Path as Path
import Prelude (Unit, bind, compare, compose, conj, const, eq, flip, map, otherwise, pure, show)

filterDir
  :: forall e
  . Array FilePath
  -> Eff (exception :: EXCEPTION, fs :: FS | e) (Array FilePath)
filterDir files = do
  maybes <- for files \f -> do
    s <- stat f
    pure (guard (isDirectory s) (Just f))
  pure (Array.catMaybes maybes)

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
getPrevFile root file = go root file [] (maybe root dirname file)
  where
    prevFilter = Array.filter (maybe (const true) greaterThan file)
    go root file dirs dir | conj (eq (Array.find (eq dir) dirs) Nothing) (startsWith root dir) = do
      files <- readDir' dir
      let filtered = Array.sortBy (flip compare) (prevFilter files) -- order by desc
      case Array.find (compose (eq ".json") extname) filtered of
        Just f -> pure (Just f)
        Nothing -> do
          -- search child dir
          childDirs <- filterDir filtered
          let
            g Nothing d = go root Nothing (append dirs [dir]) d
            g m@(Just a) _ = pure m
          m <- foldM g Nothing childDirs
          -- search parent dir
          case m of
            Just f -> pure (Just f)
            Nothing -> go root Nothing (append dirs [dir]) (dirname dir)
    go _ _ _ _ | otherwise = pure Nothing

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
