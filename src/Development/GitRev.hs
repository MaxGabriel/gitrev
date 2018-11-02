{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

-- |
-- Module      :  $Header$
-- Copyright   :  (c) 2015 Adam C. Foltzer
-- License     :  BSD3
-- Maintainer  :  acfoltzer@galois.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Some handy Template Haskell splices for including the current git
-- hash and branch in the code of your project. Useful for including
-- in panic messages, @--version@ output, or diagnostic info for more
-- informative bug reports.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import Development.GitRev
-- >
-- > panic :: String -> a
-- > panic msg = error panicMsg
-- >   where panicMsg =
-- >           concat [ "[panic ", $(gitBranch), "@", $(gitHash)
-- >                  , " (", $(gitCommitDate), ")"
-- >                  , " (", $(gitCommitCount), " commits in HEAD)"
-- >                  , dirty, "] ", msg ]
-- >         dirty | $(gitDirty) = " (uncommitted files present)"
-- >               | otherwise   = ""
-- >
-- > main = panic "oh no!"
--
-- > % cabal exec runhaskell Example.hs
-- > Example.hs: [panic master@2ae047ba5e4a6f0f3e705a43615363ac006099c1 (Mon Jan 11 11:50:59 2016 -0800) (14 commits in HEAD) (uncommitted files present)] oh no!

module Development.GitRev
  ( gitHash
  ) where

import Control.Exception
import Control.Monad
import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.Exit
import System.FilePath
import System.Process

import Prelude ()
import Prelude.Compat

-- | Run git with the given arguments and no stdin, returning the
-- stdout output. If git isn't available or something goes wrong,
-- return the second argument.
runGit :: [String] -> String -> IndexUsed -> Q (String, String)
runGit args def useIdx = do
  let oops :: SomeException -> IO (ExitCode, String, String)
      oops _e = return (ExitFailure 1, def, "oops")
  gitFound <- runIO $ isJust <$> findExecutable "git"
  if gitFound
    then do
      -- a lot of bookkeeping to record the right dependencies
      pwd <- runIO getDotGit
      let hd         = pwd </> ".git" </> "HEAD"
          index      = pwd </> ".git" </> "index"
          packedRefs = pwd </> ".git" </> "packed-refs"
      hdExists  <- runIO $ doesFileExist hd
      when hdExists $ do
        -- the HEAD file either contains the hash of a detached head
        -- or a pointer to the file that contains the hash of the head
        splitAt 5 `fmap` runIO (readFile hd) >>= \case
          -- pointer to ref
          ("ref: ", relRef) -> do
            let ref = pwd </> ".git" </> relRef
            refExists <- runIO $ doesFileExist ref
            when refExists $ addDependentFile ref
          -- detached head
          _hash -> addDependentFile hd
      -- add the index if it exists to set the dirty flag
      indexExists <- runIO $ doesFileExist index
      when (indexExists && useIdx == IdxUsed) $ addDependentFile index
      -- if the refs have been packed, the info we're looking for
      -- might be in that file rather than the one-file-per-ref case
      -- handled above
      packedExists <- runIO $ doesFileExist packedRefs
      when packedExists $ addDependentFile packedRefs
      runIO $ do
        (code, out, err) <- readProcessWithExitCode "git" args "" `catch` oops
        case code of
          ExitSuccess   -> return ("", takeWhile (/= '\n') out)
          ExitFailure badCode -> return (err, show badCode <> " " <> show err)
    else return ("git executable not found", def)

-- | Determine where our @.git@ directory is, in case we're in a
-- submodule.
getDotGit :: IO FilePath
getDotGit = do
  pwd <- getGitRoot
  let dotGit = pwd </> ".git"
      oops = return dotGit -- it's gonna fail, that's fine
  isDir <- doesDirectoryExist dotGit
  isFile <- doesFileExist dotGit
  if | isDir -> return dotGit
     | not isFile -> oops
     | isFile ->
         splitAt 8 `fmap` readFile dotGit >>= \case
           ("gitdir: ", relDir) -> do
             isRelDir <- doesDirectoryExist relDir
             if isRelDir
               then return relDir
               else oops
           _ -> oops

-- | Get the root directory of the Git repo.
getGitRoot :: IO FilePath
getGitRoot = do
  pwd <- getCurrentDirectory
  (code, out, _) <-
    readProcessWithExitCode "git" ["rev-parse", "--show-toplevel"] ""
  case code of
    ExitSuccess   -> return $ takeWhile (/= '\n') out
    ExitFailure _ -> return pwd -- later steps will fail, that's fine

-- | Type to flag if the git index is used or not in a call to runGit
data IndexUsed = IdxUsed -- ^ The git index is used
               | IdxNotUsed -- ^ The git index is /not/ used
    deriving (Eq)

-- | Return the hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository
gitHash :: ExpQ
gitHash = do
  (err, res) <- runGit ["rev-parse", "HEAD"] "UNKNOWN" IdxNotUsed
  stringE (err <> " " <> res)
