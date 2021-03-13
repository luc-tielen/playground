
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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

type Var = String
type Value = Int

data Statement
  = Block [Statement]
  | Assign Var Value
  deriving Show



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

algorithm :: DSL NameShadowing 'Definition ()
algorithm = do
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
run stmt = runSouffleInterpreted NameShadowing algorithm $ \case
  Nothing -> do
    liftIO $ print "Failed to load Souffle"
    pure []
  Just prog -> do
    extractFacts prog stmt
    S.run prog
    S.getFacts prog

extractFacts :: S.Handle NameShadowing -> Statement -> S.SouffleM ()
extractFacts prog = flip runReaderT 0 . f where
  f = \case
    Block stmts -> local (+1) $ do
      currentScope <- ask
      when (currentScope > 0) $ do
        let prevScope = currentScope - 1
        S.addFact prog $ NestedScope prevScope currentScope
      traverse_ f stmts
    Assign variable _ -> do
      currentScope <- ask
      S.addFact prog $ Define currentScope variable

main :: IO ()
main = for_ scenarios $ \scenario -> do
  print "#################"
  print scenario
  names <- run scenario
  print names
