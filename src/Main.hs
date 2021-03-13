
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Main ( main ) where


{-
Name shadowing:

Scenario 1: declaration in a nested scope

{   // Scope 0
  x = 42       <- this should give a warning
  {  // Scope 1
    x = 1000   <- this should give a warning
  }
}

Scenario 2: declaration in same scope (mutable assignment)

{   // Scope 0
  x = 42
  x = 1000     <- This should give a warning
}
-}

import Language.Souffle.Experimental
import qualified Language.Souffle.Interpreted as S
import Data.Foldable
import Control.Monad.IO.Class
import GHC.Generics
import Data.Int
import Control.Monad.Reader
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Control.Monad.State

type Var = String
type Value = Int

data Statement
  = Block [Statement]
  | Assign Var Value
  | Print Var
  deriving Show

makeBaseFunctor ''Statement


scenarios :: [Statement]
scenarios =
  [ Assign "x" 42
  , Block
    [ Assign "x" 42  -- shadowed in same scope, because of mutable reassignment
    , Assign "x" 42
    ]
  , Block
    [ Assign "x" 42  -- shadowed in nested scope
    , Block
      [ Assign "x" 1000
      , Assign "y" 2000
      ]
    , Assign "z" 100
    ]
  , Block
    [ Block
      [ Assign "z" 2000  -- Not shadowed! (requires CFG / Successor fact)
      ]
    , Assign "z" 100
    ]
  , Print "x"  -- Unbound
  , Block
    [ Print "x"  -- Unbound (use before define)
    , Assign "x" 1
    ]
  , Block
    [ Assign "x" 42
    , Block
      [ Assign "x" 42  -- shadowed in same scope, because of mutable reassignment
      ]
    , Print "y"
    ]
  ]

data NameShadowing = NameShadowing

data Shadowed = Shadowed String
  deriving (Generic, Show, FactMetadata)

type Scope = Int32

data Define = Define Scope Var
  deriving (Generic, Show, FactMetadata)

-- NOTE: subscope is nested in scope
data NestedScope
  = NestedScope
  { _scope :: Scope
  , _subscope :: Scope
  } deriving (Generic, Show, FactMetadata)

instance S.Program NameShadowing where
  type ProgramFacts NameShadowing = '[Shadowed, Define, NestedScope]
  programName = const "name_shadowing"

instance S.Fact Shadowed where
  type FactDirection Shadowed = 'S.Output
  factName = const "shadowed"

instance S.Fact Define where
  type FactDirection Define = 'S.Input
  factName = const "declare_var"

instance S.Fact NestedScope where
  type FactDirection NestedScope = 'S.Input
  factName = const "nested_scope"

instance S.Marshal Shadowed
instance S.Marshal Define
instance S.Marshal NestedScope

shadowingAlgorithm :: DSL NameShadowing 'Definition ()
shadowingAlgorithm = do
  Predicate shadowed <- predicateFor @Shadowed
  Predicate define <- predicateFor @Define
  Predicate nestedScope <- predicateFor @NestedScope

  v <- var "variable"
  scope <- var "scope"
  subscope <- var "subscope"

  shadowed(v) |- do
    nestedScope(scope, subscope)
    define(scope, v)
    define(subscope, v)

run :: Statement -> IO [Shadowed]
run stmt = runSouffleInterpreted NameShadowing shadowingAlgorithm $ \case
  Nothing -> do
    liftIO $ print "Failed to load Souffle"
    pure []
  Just prog -> do
    extractFacts prog stmt
    S.run prog
    S.getFacts prog

extractFacts :: S.Handle NameShadowing -> Statement -> S.SouffleM ()
extractFacts prog = flip runReaderT 0 . cata alg where
  -- TODO: figure out way to have Env the same for all static analysis passes?
  alg :: StatementF (ReaderT Scope S.SouffleM a) -> ReaderT Scope S.SouffleM ()
  alg = \case
    BlockF actions -> local (+1) $ do
      currentScope <- ask
      when (currentScope > 0) $ do
        let prevScope = currentScope - 1
        S.addFact prog $ NestedScope prevScope currentScope
      sequence_ actions
    AssignF variable _ -> do
      currentScope <- ask
      S.addFact prog $ Define currentScope variable
    _ -> pure ()


data UnboundVariable = UnboundVariable

type Line = Int32

data Use = Use Var Line
  deriving (Generic, FactMetadata)

data Define' = Define' Var Line
  deriving (Generic, FactMetadata)

data Unbound = Unbound Var Line
  deriving (Generic, Show, FactMetadata)

instance S.Program UnboundVariable where
  type ProgramFacts UnboundVariable = '[Unbound, Use, Define']
  programName = const "unbound_variable"

instance S.Fact Use where
  type FactDirection Use = 'S.Input
  factName = const "use"

instance S.Fact Define' where
  type FactDirection Define' = 'S.Input
  factName = const "define"

instance S.Fact Unbound where
  type FactDirection Unbound = 'S.Output
  factName = const "unbound"

instance S.Marshal Use
instance S.Marshal Define'
instance S.Marshal Unbound

unboundVarAlgorithm :: DSL UnboundVariable 'Definition ()
unboundVarAlgorithm = do
  Predicate unbound <- predicateFor @Unbound
  Predicate use <- predicateFor @Use
  Predicate define <- predicateFor @Define'

  v <- var "variable"
  line1 <- var "line1"
  line2 <- var "line2"

  unbound(v, line1) |- do
    use(v, line1)
    not' $ define(v, __)

  unbound(v, line1) |- do
    use(v, line1)
    define(v, line2)
    line1 .< line2

-- TODO merge with run
run' :: Statement -> IO [Unbound]
run' stmt = runSouffleInterpreted UnboundVariable unboundVarAlgorithm $ \case
  Nothing -> do
    liftIO $ print "Failed to load Souffle"
    pure []
  Just prog -> do
    extractFacts' prog stmt
    S.run prog
    S.getFacts prog

-- TODO merge with extractFacts'
extractFacts' :: S.Handle UnboundVariable -> Statement -> S.SouffleM ()
extractFacts' prog stmt = flip evalStateT 0 $ alg stmt where
  alg stmt' = incr *> alg' stmt'
  alg' = \case
    Block stmts -> traverse_ alg stmts
    Assign variable _ -> do
      line <- get
      S.addFact prog $ Define' variable line
    Print variable -> do
      line <- get
      S.addFact prog $ Use variable line
  incr = modify (+1)


main :: IO ()
main = for_ scenarios $ \scenario -> do
  print "#################"
  print scenario
  shadowedNames <- run scenario
  unboundVars <- run' scenario
  print (shadowedNames, unboundVars)
