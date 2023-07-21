{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH.Syntax
import Control.Monad
import Distribution.Simple
import Distribution.System (OS (..), buildOS)
import Distribution.Types.LocalBuildInfo
import System.Directory (copyFile, doesFileExist, withCurrentDirectory)
import System.FilePath ((<.>), (</>))
import System.Process (system)

[] <$ qAddDependentFile "cbits/path.ll"

-- TODO check for clang (+ version) also
main :: IO ()
main = do
  defaultMainWithHooks $
    simpleUserHooks
      { postConf = \_args _configFlags _packageDescription localBuildInfo -> do
          -- Cabal, and indeed, GHC, don't understand the .lib extension on
          -- Windows, so we have the same name everywhere.
          let destinationPath = buildDir localBuildInfo </> "libpath" <.> "a"
          case buildOS of
            Windows -> putStrLn "Sorry, Eclair is not supported on Windows (yet)!"
            _ -> do
              -- Since everything else is some flavour of POSIX, we can use the
              -- Autotools to build in-place. This current (more involved) setup
              -- avoids triggering unnecessary rebuilds by checking if configure
              -- and/or make already ran.
              let workPath = "cbits"
                  eclairProgramPath = workPath </> "libpath.a"
              isAlreadyCompiled <- doesFileExist eclairProgramPath
              unless isAlreadyCompiled $ withCurrentDirectory workPath $ do
                  void . system $ "clang -c -o path.o path.ll"
                  void . system $ "ar rcs libpath.a path.o"
              copyFile eclairProgramPath destinationPath
      }
