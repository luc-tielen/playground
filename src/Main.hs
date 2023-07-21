{-# LANGUAGE DataKinds, DeriveGeneric, DeriveAnyClass, DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}  -- TODO rm
-- UndecidableInstances is only needed for the DerivingVia style API.

module Main ( main ) where

import qualified Language.Eclair as E
import Language.Haskell.TH.Syntax
import GHC.Generics
import Data.Word
import Data.Foldable

-- TODO rm
-- [] <$ qAddForeignFilePath RawObject "cbits/path.o"

data Edge
  = Edge Word32 Word32
  deriving (Generic)
  deriving anyclass E.Marshal
  deriving E.Fact via E.FactOptions Edge 'E.Input "edge"

data Reachable
  = Reachable Word32 Word32
  deriving (Show, Generic)
  deriving anyclass E.Marshal
  deriving E.Fact via E.FactOptions Reachable 'E.Output "reachable"

data Path = Path
  deriving E.Program
  via E.ProgramOptions Path '[Edge, Reachable]

main :: IO ()
main = do
  putStrLn "Starting..."
  results <- E.withEclair Path $ \prog -> do
    E.addFacts prog [Edge 1 2, Edge 2 3]
    E.addFact prog $ Edge 4 5
    E.run prog
    E.getFacts prog
  process results
  putStrLn "Done!"
  where
    process :: [Reachable] -> IO ()
    process = traverse_ print
