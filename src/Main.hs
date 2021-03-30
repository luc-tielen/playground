
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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



data UnboundVariable = UnboundVariable

type Scope = Int32
type Line = Int32

newtype Scope' = Scope' Scope
newtype LineNr = LineNr Line

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

data Env
  = Env
  { _envScope :: Scope'
  , _envLine :: LineNr
  }

run :: Statement -> IO ([Shadowed], [Unbound])
run stmt = go where
  go = runSouffleInterpreted NameShadowing shadowingAlgorithm $ \case
    Nothing -> abort
    Just prog -> do
      runSouffleInterpreted UnboundVariable unboundVarAlgorithm $ \case
        Nothing -> abort
        Just prog' -> runAlgorithms prog prog'
  abort = do
    liftIO $ print "Failed to load Souffle"
    pure ([], [])
  runAlgorithms prog prog' = do
    let action = cata (compose (algNS prog) (algUV prog')) stmt
        env = Env (Scope' 0) (LineNr 0)
    flip runReaderT env action
    S.run prog
    S.run prog'
    (,) <$> S.getFacts prog <*> S.getFacts prog'

compose :: (Applicative f, Semigroup b)
        => (a -> f b)
        -> (a -> f b)
        -> a -> f b
compose f g a = (<>) <$> f a <*> g a

algNS :: S.Handle NameShadowing
      -> StatementF (ReaderT Env S.SouffleM a)
      -> ReaderT Env S.SouffleM ()
algNS prog = \case
  BlockF actions -> local newScope $ do
    Scope' currentScope <- asks _envScope
    when (currentScope > 0) $ do
      let prevScope = currentScope - 1
      S.addFact prog $ NestedScope prevScope currentScope
    sequence_ actions
  AssignF variable _ -> do
    Scope' currentScope <- asks _envScope
    S.addFact prog $ Define currentScope variable
  _ -> pure ()
  where newScope (Env (Scope' s) line) = Env (Scope' (s + 1)) line

algUV :: S.Handle UnboundVariable
      -> StatementF (ReaderT Env S.SouffleM a)
      -> ReaderT Env S.SouffleM ()
algUV prog = local incrLine . \case
  BlockF actions -> sequence_ actions
  AssignF variable _ -> do
    LineNr line <- asks _envLine
    S.addFact prog $ Define' variable line
  PrintF variable -> do
    LineNr line <- asks _envLine
    S.addFact prog $ Use variable line
  where incrLine (Env s (LineNr line)) = Env s (LineNr (line + 1))



--extractFacts' :: S.Handle UnboundVariable -> Statement -> S.SouffleM ()
--extractFacts' prog stmt = flip runReaderT 0 $ alg stmt where
  --alg stmt' = local (+1) $ alg' stmt'
  --alg' = \case
    --Block stmts -> traverse_ alg stmts
    --Assign variable _ -> do
      --line <- ask
      --S.addFact prog $ Define' variable line
    --Print variable -> do
      --line <- ask
      --S.addFact prog $ Use variable line


main :: IO ()
main = for_ scenarios $ \scenario -> do
  print "#################"
  print scenario
  results <- run scenario
  print results
