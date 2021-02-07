{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Main ( main ) where

import Prelude hiding (succ)
import qualified Language.Souffle.Interpreted as Souffle
import Language.Souffle.Experimental hiding (VarName)
import Data.Foldable
import Data.Map ( Map )
import qualified Data.Map as Map
import Control.Monad.State
import GHC.Generics
import Data.Int
import Data.Maybe (catMaybes)


type Id = Int32

type SupplyT = StateT Id

fresh :: Monad m => SupplyT m Id
fresh = do
  current <- get
  modify (+1)
  pure current

runSupplyT :: Monad m => Id -> SupplyT m a -> m a
runSupplyT = flip evalStateT

annotate :: AST () -> SupplyT IO (AST Id)
annotate = \case
  Assign _ varName value -> do
    lineNr <- fresh
    pure $ Assign lineNr varName value
  Print _ varName -> do
    lineNr <- fresh
    pure $ Print lineNr varName
  Increment _ varName -> do
    lineNr <- fresh
    pure $ Increment lineNr varName
  Block _ insts -> do
    lineNr <- fresh
    Block lineNr <$> traverse annotate insts

type VarName = String
type Value = Int

data AST a
  = Assign a VarName Value
  | Increment a VarName
  | Print a VarName
  | Block a [AST a]
  deriving (Eq, Show)

assign :: VarName -> Value -> AST ()
assign = Assign ()

print_ :: VarName -> AST ()
print_ = Print ()

incr :: VarName -> AST ()
incr = Increment ()

type Env = Map VarName Value

eval :: AST a -> IO ()
eval stmt = flip evalStateT mempty $ go stmt where
  go :: AST a -> StateT Env IO ()
  go = \case
    Assign _ varName value -> modify (Map.insert varName value)
    Increment _ varName -> modify (Map.adjust (+1) varName)
    Print _ varName -> do
      value <- gets (Map.lookup varName)
      lift $ putStrLn $ show value
    Block _ insts -> traverse_ go insts

scenarios :: [AST ()]
scenarios = map (Block ())
  [ [ assign "x" 1
    , assign "y" 2  -- This should be deleted (dead code)
    , print_ "x"
    ]
  , [ assign "x" 1
    , assign "y" 2
    , print_ "x"
    , print_ "y"
    ]
  , [ assign "x" 1  -- Should be deleted
    , assign "x" 2
    , print_ "x"
    ]
  , [ assign "x" 1  -- Should be deleted
    , assign "x" 2  -- Should be deleted
    , assign "x" 3
    , print_ "x"
    ]
  , [ assign "x" 1
    , print_ "x"

    , assign "x" 2  -- Should be deleted
    , assign "x" 3
    , print_ "x"
    ]
  , [ assign "x" 1
    , print_ "x"

    , assign "y" 4  -- Should be deleted
    , assign "x" 2  -- Should be deleted
    , assign "x" 3
    , print_ "x"
    ]
  , [ assign "x" 1
    , print_ "x"

    , assign "y" 4
    , assign "x" 2  -- Should be deleted
    , assign "x" 3
    , print_ "x"
    , print_ "y"
    ]
  , [ assign "x" 1  -- Should be deleted (but is not? more than 1 iteration needed?)
    , incr "x"  -- Should be deleted
    , assign "x" 10
    , incr "x"
    , assign "y" 2  -- Should be deleted
    , print_ "x"
    ]
  , [ assign "x" 1 ]
  ]



algorithm :: DSL DCE 'Definition ()
algorithm = do
  Predicate define <- predicateFor @Define
  Predicate use <- predicateFor @Use
  Predicate succ <- predicateFor @Succ
  Predicate live <- predicateFor @Live
  Predicate deadCode <- predicateFor @DeadCode

  lineNr <- var "lineNr1"
  lineNr2 <- var "lineNr2"
  varName <- var "varName"

  live(lineNr, varName) |- do
    succ(lineNr, lineNr2)
    use(lineNr2, varName)
  live(lineNr, varName) |- do
    succ(lineNr, lineNr2)
    live(lineNr2, varName)
    not' $ define(lineNr2, varName)

  deadCode(lineNr) |- do
    define(lineNr, varName)
    not' $ live(lineNr, varName)

data DCE = DCE

type LineNr = Int32

data Define = Define LineNr VarName
  deriving (Generic, FactMetadata)

data Use = Use LineNr VarName
  deriving (Generic, FactMetadata)

data Live = Live LineNr VarName
  deriving (Generic, FactMetadata, Show)

data Succ = Succ LineNr LineNr
  deriving (Generic, FactMetadata)

data DeadCode = DeadCode LineNr
  deriving (Generic, Show, FactMetadata)

instance Souffle.Program DCE where
  type ProgramFacts DCE = '[Define, Use, Live, DeadCode, Succ]
  programName = const "dce"

instance Souffle.Fact Define where
  type FactDirection Define = 'Souffle.Input
  factName = const "define"

instance Souffle.Fact Use where
  type FactDirection Use = 'Souffle.Input
  factName = const "use"

instance Souffle.Fact Live where
  type FactDirection Live = 'Souffle.Internal
  factName = const "live"

instance Souffle.Fact Succ where
  type FactDirection Succ = 'Souffle.Input
  factName = const "succ"

instance Souffle.Fact DeadCode where
  type FactDirection DeadCode = 'Souffle.Output
  factName = const "deadcode"

instance Souffle.Marshal Define
instance Souffle.Marshal Use
instance Souffle.Marshal DeadCode
instance Souffle.Marshal Live
instance Souffle.Marshal Succ

dce :: AST Id -> IO (Maybe (AST Id))
dce stmt = do
  runSouffleInterpreted DCE algorithm $ \case
    Nothing -> do
      liftIO $ putStrLn "Failed to load Souffle"
      pure $ Just stmt
    Just prog -> do
      extractFacts prog stmt
      Souffle.run prog
      deadInsts <- Souffle.getFacts prog
      pure $ simplify deadInsts stmt

extractFacts :: Souffle.Handle DCE -> AST Id -> Souffle.SouffleM ()
extractFacts prog = \case
  Assign lineNr varName _ ->
    Souffle.addFact prog $ Define lineNr varName
  Increment lineNr varName -> do
    Souffle.addFact prog $ Define lineNr varName
    Souffle.addFact prog $ Use lineNr varName
  Print lineNr varName ->
    Souffle.addFact prog $ Use lineNr varName
  Block _ insts -> do
    traverse_ (extractFacts prog) insts
    let lineNrs = map getId insts
        successors = uncurry Succ <$> zip lineNrs (drop 1 lineNrs)
    Souffle.addFacts prog successors

getId :: AST Id -> Id
getId = \case
  Assign i _ _ -> i
  Increment i _ -> i
  Print i _ -> i
  Block i _ -> i

simplify :: [DeadCode] -> AST Id -> Maybe (AST Id)
simplify deadInsts stmt = case stmt of
  Block nodeId insts ->
    simplify' stmt $
      case catMaybes $ simplify deadInsts <$> insts of
        [] -> Nothing
        simplifiedInsts -> Just $ Block nodeId simplifiedInsts
  _ -> simplify' stmt (Just stmt)
  where deadLineNrs = [i | DeadCode i <- deadInsts]
        simplify' original modified =
          let nodeId = getId original
           in if nodeId `elem` deadLineNrs
                then Nothing
                else modified

main :: IO ()
main = for_ scenarios $ \program -> do
  annotated  <- runSupplyT 0 (annotate program)
  optimizedProgram <- dce annotated
  case optimizedProgram of
    Nothing -> print "Optimized away!"
    Just optimizedProgram' -> do
      print optimizedProgram'
      eval optimizedProgram'

