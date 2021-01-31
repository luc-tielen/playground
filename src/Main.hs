{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Main ( main ) where

import qualified Language.Souffle.Interpreted as Souffle
import Language.Souffle.Experimental hiding (VarName)
import Data.Foldable
import Data.Map ( Map )
import qualified Data.Map as Map
import Control.Monad.State
import GHC.Generics
import Data.Int

type Program = [Instruction]

type VarName = String
type Value = Int

data Instruction
  = Assign VarName Value
  | Print VarName
  deriving Show

type Env = Map VarName Value

eval :: Program -> IO ()
eval instructions = flip evalStateT mempty $ traverse_ go instructions where
  go :: Instruction -> StateT Env IO ()
  go = \case
    Assign varName value -> modify (Map.insert varName value)
    Print varName -> do
      value <- gets (Map.lookup varName)
      lift $ putStrLn $ show value

scenarios :: [Program]
scenarios =
  [ [ Assign "x" 1
    , Assign "y" 2  -- This should be deleted (dead code)
    , Print "x"
    ]
  , [ Assign "x" 1
    , Assign "y" 2
    , Print "x"
    , Print "y"
    ]
  , [ Assign "x" 1  -- Should be deleted
    , Assign "x" 2
    , Print "x"
    ]
  , [ Assign "x" 1  -- Should be deleted
    , Assign "x" 2  -- Should be deleted
    , Assign "x" 3
    , Print "x"
    ]
  , [ Assign "x" 1
    , Print "x"

    , Assign "x" 2  -- Should be deleted
    , Assign "x" 3
    , Print "x"
    ]
  ]


algorithm :: DSL DCE 'Definition ()
algorithm = do
  Predicate define <- predicateFor @Define
  Predicate use <- predicateFor @Use
  Predicate deadCode <- predicateFor @DeadCode
  Predicate badInstances <- predicateFor @BadInstances

  lineNr <- var "lineNr"
  lineNr2 <- var "lineNr2"
  lineNr3 <- var "lineNr3"
  varName <- var "varName"

  deadCode(lineNr) |- do
    define(lineNr, varName)
    not' $ use(__, varName)
  deadCode(lineNr) |- do
    define(lineNr, varName)
    define(lineNr2, varName)
    lineNr .< lineNr2
    not' $ badInstances(lineNr)

  badInstances(lineNr) |- do
    define(lineNr, varName)
    use(lineNr3, varName)
    define(lineNr2, varName)
    lineNr .< lineNr2
    lineNr .< lineNr3
    lineNr3 .< lineNr2


data DCE = DCE

type LineNr = Int32

data Define = Define LineNr VarName
  deriving (Generic, FactMetadata)

data Use = Use LineNr VarName
  deriving (Generic, FactMetadata)

data DeadCode = DeadCode LineNr
  deriving (Generic, Show, FactMetadata)

data BadInstances = BadInstances LineNr
  deriving (Generic, Show, FactMetadata)

instance Souffle.Program DCE where
  type ProgramFacts DCE = '[Define, Use, DeadCode, BadInstances]
  programName = const "dce"

instance Souffle.Fact Define where
  type FactDirection Define = 'Souffle.Input
  factName = const "define"

instance Souffle.Fact Use where
  type FactDirection Use = 'Souffle.Input
  factName = const "use"

instance Souffle.Fact BadInstances where
  type FactDirection BadInstances = 'Souffle.Input
  factName = const "bad_instances"

instance Souffle.Fact DeadCode where
  type FactDirection DeadCode = 'Souffle.Output
  factName = const "deadcode"

instance Souffle.Marshal Define
instance Souffle.Marshal Use
instance Souffle.Marshal DeadCode
instance Souffle.Marshal BadInstances

dce :: Program -> IO Program
dce instructions = do
  renderIO DCE "/tmp/algorithm.dl" algorithm
  runSouffleInterpreted DCE algorithm $ \case
    Nothing -> do
      liftIO $ putStrLn "Failed to load Souffle"
      pure instructions
    Just prog -> do
      let program' = zip [0..] instructions
      traverse_ extractFacts program'
      Souffle.run prog
      deadInsts <- Souffle.getFacts prog
      pure $ simplify deadInsts program'
      where extractFacts (lineNr, inst) = case inst of
              Assign varName _ ->
                Souffle.addFact prog $ Define lineNr varName
              Print varName ->
                Souffle.addFact prog $ Use lineNr varName
            simplify :: [DeadCode] -> [(LineNr, Instruction)] -> Program
            simplify deadInsts =
              let deadLineNrs = [i | DeadCode i <- deadInsts]
              in map snd . filter ((`notElem` deadLineNrs) . fst)

main :: IO ()
main = for_ scenarios $ \program -> do
  optimizedProgram <- dce program
  print optimizedProgram
  eval optimizedProgram


