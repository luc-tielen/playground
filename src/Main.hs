{-# LANGUAGE DataKinds, TypeFamilies, DeriveGeneric #-}

module Main ( main ) where

import Language.Souffle.Interpreted as S
import GHC.Generics
import Data.Int
import Data.Foldable
import Control.Monad
import Control.Monad.Reader

type Var = String

type Value = Int

data Expr
  = Block [Expr]
  | Assign Var Value

exampleExpr :: Expr
exampleExpr =
  Block
    [ Assign "x" 123
    , Block
      [ Assign "x" 456
      ]
    ]

-- A type for representing our Datalog program:
data NameShadowing = NameShadowing

-- Types that correspond with our Datalog facts:

type Scope = Int32

data Define = Define Scope Var
  deriving Generic

data NestedScope
  = NestedScope
  { _scope :: Scope
  , _subscope :: Scope
  } deriving Generic

data Shadowed = Shadowed Var
  deriving (Generic, Show)

-- Some instances for communicating between Haskell and Datalog:

instance S.Program NameShadowing where
  type ProgramFacts NameShadowing = '[Shadowed, Define, NestedScope]
  programName = const "name_shadowing"

instance S.Fact Define where
  type FactDirection Define = 'S.Input
  factName = const "define"

instance S.Fact NestedScope where
  type FactDirection NestedScope = 'S.Input
  factName = const "nested_scope"

instance S.Fact Shadowed where
  type FactDirection Shadowed = 'S.Output
  factName = const "shadowed"

instance S.Marshal Define
instance S.Marshal NestedScope
instance S.Marshal Shadowed

deduceFacts :: S.Handle NameShadowing -> Expr -> S.SouffleM ()
deduceFacts handle expr = runReaderT (go expr) rootScope
  where
    -- Our function needs to keep track of which scope it is in.
    -- Scope at root level is 0, every nested scope increases by 1.
    -- We can use the Reader monad for keeping track of scope since
    -- we only ever modify it locally and for the rest only
    -- perform reads.
    rootScope = 0
    newScope s = s + 1
    go e = case e of
      -- Assignments are straight-forward: we lookup the current scope
      -- and store the fact that a variable is defined at this scope.
      Assign var _value -> do
        currentScope <- ask
        S.addFact handle $ Define currentScope var
      -- Blocks are more complicated since the scope changes when you
      -- enter a block. We can use Reader here for easy access to the
      -- scope info. If we are in a nested scope, then we can submit
      -- this fact to Datalog as well.
      Block exprs -> local newScope $ do
        currentScope <- ask
        when (currentScope > rootScope) $ do
          let prevScope = currentScope - 1
          S.addFact handle $ NestedScope prevScope currentScope
        -- Don't forget to handle the nested sub-expressions:
        traverse_ go exprs

main :: IO ()
main = S.runSouffle NameShadowing $ \case
  Nothing ->
    liftIO $ putStrLn "Failed to load program."
  Just prog -> do
    deduceFacts prog exampleExpr
    S.run prog
    shadowedVars :: [Shadowed] <- S.getFacts prog
    -- Here we simply print out the results, but you could theoretically
    -- use the results for error reporting, optimizations, ...
    liftIO $ do
      putStrLn "Shadowed variables in expression:"
      traverse_ print shadowedVars
