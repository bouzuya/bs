module Lib
  ( getPrevFile
  , getPrevFile'
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as Array
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Monoid (append)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Ord (greaterThan)
import Data.String as String
import Node.FS (FS)
import Node.FS.Stats (isDirectory)
import Node.FS.Sync (readdir, stat)
import Node.Path (FilePath, dirname, extname)
import Node.Path as Path
import Prelude (bind, compare, compose, conj, const, eq, flip, map, not, otherwise, pure)

readDir'
  :: forall e
  . FilePath -> Eff (exception :: EXCEPTION, fs :: FS | e) (Array FilePath)
readDir' dir = map (map (compose Path.concat (Array.snoc [dir]))) (readdir dir)

startsWith :: String -> String -> Boolean
startsWith p = compose (eq (Just 0)) (String.indexOf (String.Pattern p))

getPrevFile'
  :: forall e
  . FilePath
  -> Maybe FilePath
  -> Eff
    (exception :: EXCEPTION, fs :: FS | e)
    (Maybe FilePath)
getPrevFile' root file = go root file [] (maybe root dirname file)
  where
    contains x xs = isJust (Array.find (eq x) xs)
    prevFilter = Array.filter (maybe (const true) greaterThan file)
    go root' file dirs dir
      | conj (startsWith root' dir) (not (contains dir dirs)) = do
        files <- readDir' dir
        let
          -- order by desc
          filtered = Array.sortBy (flip compare) (prevFilter files)
          nextDirs = append dirs [dir]
        -- search current dir (and child dir)
        currentDirResult <- foldM
          (\result f ->
            case result of
              Just found -> pure (Just found)
              Nothing -> do
                s <- stat f
                if isDirectory s
                  then go root' Nothing nextDirs f -- search child dir
                  else pure if eq ".json" (extname f)
                    then Just f
                    else Nothing
          )
          Nothing
          filtered
        case currentDirResult of
          Just found -> pure (Just found)
          Nothing -> do -- search parent dir
            go root Nothing nextDirs (dirname dir)
    go _ _ _ _
      | otherwise = pure Nothing

getPrevFile
  :: forall e
  . FilePath
  -> Nullable FilePath
  -> Eff
    (exception :: EXCEPTION, fs :: FS | e)
    (Nullable FilePath)
getPrevFile root file =
  map toNullable (getPrevFile' root (toMaybe file))
