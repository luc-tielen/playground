{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DataKinds, TypeFamilies, LambdaCase #-}

module Main where

import qualified Language.Souffle.Interpreted as Souffle
import GHC.Generics
import Control.Monad.IO.Class

-- TODO: experiment with derivingvia to codegen program instances


data Categorizer = Categorizer

instance Souffle.Program Categorizer where
  type ProgramFacts Categorizer =
    '[ UserPackageCategory
     , NormalizedPackageCategory
     , NormalizeIssue
     ]

  programName = const "categorize"

type PackageName = String
type Category = String -- TODO: better name

data UserPackageCategory
  = UserPackageCategory PackageName Category
  deriving (Generic, Show)

data NormalizedPackageCategory
  = NormalizedPackageCategory PackageName Category
  deriving (Generic, Show)

data NormalizeIssue
  = NormalizeIssue PackageName Category
  deriving (Generic, Show)

data Results
  = Results [NormalizedPackageCategory] [NormalizeIssue]
  deriving Show

instance Souffle.Marshal UserPackageCategory
instance Souffle.Marshal NormalizedPackageCategory
instance Souffle.Marshal NormalizeIssue

instance Souffle.Fact UserPackageCategory where
  type FactDirection UserPackageCategory = 'Souffle.Input
  factName = const "user_package_category"

instance Souffle.Fact NormalizedPackageCategory where
  type FactDirection NormalizedPackageCategory = 'Souffle.Output
  factName = const "normalized_package_category"

instance Souffle.Fact NormalizeIssue where
  type FactDirection NormalizeIssue = 'Souffle.Output
  factName = const "normalize_issue"

input :: [UserPackageCategory]
input =
  []  -- Fill as needed!

main :: IO ()
main = Souffle.runSouffle Categorizer $ \case
  Nothing -> liftIO $ print "Failed to load Souffle program!"
  Just prog -> do
    Souffle.addFacts prog input
    Souffle.run prog
    results <- Results <$> Souffle.getFacts prog <*> Souffle.getFacts prog
    liftIO $ print results


