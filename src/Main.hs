{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module Main ( main ) where

import Prelude hiding (succ)
import qualified Language.Souffle.Interpreted as Souffle
import Language.Souffle.Experimental hiding (VarName)
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import GHC.Generics
import Data.Int
import Data.Maybe (mapMaybe, listToMaybe)

-- First, we define a statement-based language

type VarName = String
type Value = Int

-- Currently we only have a single type of expression: a variable (lookup)
type Expr = VarName

-- Type variable is used for annotations. Annotations start empty, but are then
-- filled with unique IDs in a compiler pass. In a real compiler, this could be
-- done during the parsing stage.
data Instruction a
  = Assign a VarName Value
  | Increment a VarName
  | Print a VarName
  | If a Expr [Instruction a] [Instruction a]
  deriving (Eq, Show)

type Program a = [Instruction a]

assign :: VarName -> Value -> Instruction ()
assign = Assign ()

print_ :: VarName -> Instruction ()
print_ = Print ()

if_ :: VarName -> [Instruction ()] -> [Instruction ()] -> Instruction ()
if_ = If ()

incr :: VarName -> Instruction ()
incr = Increment ()

-- We define a "Supply" monad, for supplying us with unique IDs
type Id = Int32

type SupplyT = StateT Id

fresh :: Monad m => SupplyT m Id
fresh = do
  current <- get
  modify (+1)
  pure current

runSupplyT :: Monad m => Id -> SupplyT m a -> m a
runSupplyT = flip evalStateT

-- Annotates the language with unique node IDs
annotate :: Instruction () -> SupplyT IO (Instruction Id)
annotate = \case
  Assign _ varName value -> do
    nodeId <- fresh
    pure $ Assign nodeId varName value
  Print _ varName -> do
    nodeId <- fresh
    pure $ Print nodeId varName
  Increment _ varName -> do
    nodeId <- fresh
    pure $ Increment nodeId varName
  If _ c t f -> do
    nodeId <- fresh
    If nodeId c <$> traverse annotate t <*> traverse annotate f



data DCE = DCE

type LineNr = Int32

data Define = Define LineNr VarName
  deriving (Generic, FactMetadata)

data Use = Use LineNr VarName
  deriving (Generic, FactMetadata)

data Live = Live LineNr VarName
  deriving (Generic, FactMetadata, Show)

data Succ = Succ LineNr LineNr
  deriving (Show, Generic, FactMetadata)

data DeadCode = DeadCode LineNr
  deriving (Generic, Show, FactMetadata)

instance Souffle.Program DCE where
  type ProgramFacts DCE = '[Define, Use, Live, Succ, DeadCode]
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

-- The Datalog algorithm for computing dead code.
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

-- Optimizes a program using a dead code elimination (dce) pass.
dce :: Program Id -> IO (Program Id)
dce insts = runSouffleInterpreted DCE algorithm $ \case
  Nothing -> do
    liftIO $ putStrLn "Failed to load Souffle."
    pure insts
  Just prog -> do
    traverse_ (extractFacts prog) insts
    Souffle.addFacts prog $ successors insts
    Souffle.run prog
    deadInsts <- Souffle.getFacts prog
    pure $ simplify deadInsts insts

-- Helper function that traverses the AST and collects facts when a variable
-- is defined or used. This information is used in the live variable analysis.
extractFacts :: Souffle.Handle DCE -> Instruction Id -> Souffle.SouffleM ()
extractFacts prog = \case
  Assign nodeId varName _ ->
    Souffle.addFact prog $ Define nodeId varName
  Increment nodeId varName -> do
    Souffle.addFact prog $ Define nodeId varName
    Souffle.addFact prog $ Use nodeId varName
  Print nodeId varName ->
    Souffle.addFact prog $ Use nodeId varName
  If nodeId c t f -> do
    Souffle.addFact prog $ Use nodeId c
    traverse_ (traverse_ $ extractFacts prog) [t, f]

{-
Helper function for computing a list of successors in a program.
On the datalog side this is used to compute the live variables.

Given a snippet:

if (x == true) {      // 1
  if (y == 1000) {    // 2
    print(x);         // 3
    print(y);         // 4
  } else {
    print(y);         // 5
  }
} else {
  print(42);          // 6
}
print(z);             // 7

This should give us the following successors:
[(1, 2), (2, 3), (3, 4), (4,7), (2, 5), (5, 7), (1,6), (6, 7)]
-}
successors :: Program Id -> [Succ]
successors insts = mconcat $ zipWith g insts nextIds where
  g inst nextId =
    let direct = uncurry Succ <$> map (,nextId) (lastIds inst)
        nested = h inst
     in direct <> nested
  h = \case
    If nodeId _ t f ->
      let succsT = successors t
          succsF = successors f
          succs = Succ nodeId <$> firstIds [t, f]
      in succs <> succsT <> succsF
    _ -> []
  nextIds = drop 1 $ map getId insts
  firstIds = mapMaybe (fmap getId . listToMaybe)
  lastIds = \case
    If _ _ t f ->
      mconcat $ mapMaybe (fmap lastIds . listToMaybe . reverse) [t, f]
    inst ->
      [getId inst]

-- After the lines of dead code have been found, we can optimize (simplify) our program.
simplify :: [DeadCode] -> Program Id -> Program Id
simplify deadInsts = go [i | DeadCode i <- deadInsts] where
  go lineNrs = mapMaybe $ \inst ->
    if getId inst `elem` lineNrs
      then Nothing
      else Just (transform lineNrs inst)
  transform lineNrs = \case
    If nodeId c t f ->
      If nodeId c (go lineNrs t) (go lineNrs f)
    inst -> inst

getId :: Instruction Id -> Id
getId = \case
  Assign i _ _ -> i
  Increment i _ -> i
  Print i _ -> i
  If i _ _ _ -> i

type Env = Map VarName Value

-- An evaluator (mostly used for testing if behavior is the same after optimization)
eval :: Program a -> IO ()
eval stmt = flip evalStateT mempty $ traverse_ go stmt where
  go :: Instruction a -> StateT Env IO ()
  go = \case
    Assign _ varName value -> modify (Map.insert varName value)
    Increment _ varName -> modify (Map.adjust (+1) varName)
    Print _ varName -> do
      value <- gets (Map.lookup varName)
      lift . putStrLn $ show value
    If _ c t f -> gets (Map.lookup c) >>= \case
      Nothing -> liftIO $ putStrLn $ "Unbound variable: " <> c
      Just value ->
        if value /= 0 then traverse_ go t else traverse_ go f


-- Some test scenarios
scenarios :: [Program ()]
scenarios =
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
  , [ assign "x" 1
    , assign "y" 2
    , if_ "y"
        [assign "x" 2, assign "x" 3]
        [print_ "x"]
    , print_ "x"
    ]
  , [ assign "x" 1
    , assign "y" 2
    , if_ "y"
        [ assign "x" 2  -- Should be deleted
        , assign "x" 3
        ]
        [print_ "x"]
    , print_ "x"
    ]
  , [ assign "x" 1  -- Should be deleted
    , assign "y" 2
    , if_ "y"
        [ assign "x" 2  -- Should be deleted
        , assign "x" 3
        ]
        [assign "x" 4]
    , print_ "x"
    ]
  , [ assign "y" 1
    , if_ "y"
        [ if_ "y"
            [ assign "x" 2  -- Should be deleted
            , assign "x" 3
            ]
            [ assign "x" 4 ]
        ]
        [assign "x" 5]
    , print_ "x"
    ]
  , [ assign "y" 1
    , if_ "y"
        [assign "x" 2]  -- Should be deleted
        [assign "x" 3]  -- Should be deleted
    , assign "x" 4
    , print_ "x"
    ]
  , [ assign "y" 1
    , if_ "y"
        [assign "x" 2]
        [assign "x" 3]
    , print_ "x"
    , assign "x" 4
    , print_ "x"
    ]
  ]


main :: IO ()
main = for_ scenarios $ \program -> do
  annotated  <- runSupplyT 0 (traverse annotate program)
  optimizedProgram <- dce annotated
  print optimizedProgram
  --eval annotated
  eval optimizedProgram

