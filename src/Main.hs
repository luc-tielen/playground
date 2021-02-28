
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}  -- TODO remove
module Main ( main ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Foldable (traverse_)
import Data.Function ((&), fix)
import Data.Containers.ListUtils (nubOrd)
import Control.Arrow ((>>>))

type Variable = String

type Name = String

data Term
  = Constant String
  | Var Variable
  deriving (Eq, Ord, Show)

data Fact
  = Fact
  { factName :: Name
  , factTerms :: [Term]
  } deriving (Eq, Ord, Show)

data Rule
  = Rule
  { ruleHead :: Fact
  , ruleBody :: [Fact]
  } deriving Show

type Database = [Fact]

type Query = Fact


-- Implementation:

generateKnowledgeBase :: (Database -> Rule -> [Fact])
                      -> [Rule] -> Database -> Database
generateKnowledgeBase f rs db = nubOrd $ db ++ concatMap (f db) rs

nameMatches :: Fact -> Fact -> Bool
nameMatches f1 f2 = factName f1 == factName f2

matches :: Fact -> Fact -> Bool
matches q fact = nameMatches q fact && attributesMatch where
  attributesMatch =
    zip (factTerms q) (factTerms fact)
    & filter (fst >>> isConstant)
    & all (uncurry (==))
  isConstant Constant{} = True
  isConstant _ = False

-- Should be of type: Map Var Term -> Map Var Term -> Bool
-- This also applies to other functions below...
hasCommonValue :: Map Term Term -> Map Term Term -> Bool
hasCommonValue attrs1 attrs2 =
  let commonVars = Map.keysSet $ Map.intersection attrs1 attrs2
      attrsMatch v = Map.lookup v attrs1 == Map.lookup v attrs2
   in all attrsMatch commonVars

evaluateLogicalOperatorsInRule :: Database -> Rule -> [Fact]
evaluateLogicalOperatorsInRule db r =
  let bodyAttrs = map (matchRelationInDb db) $ ruleBody r
      attrs = conjunct bodyAttrs
      rHead = ruleHead r
      ruleName = factName rHead
      inferFact = Fact ruleName . ruleAttrs rHead
   in map inferFact attrs

-- in blog: called match_relation_and_db
matchRelationInDb :: Database -> Fact -> [Map Term Term]
matchRelationInDb db fact =
  let matchingFacts = filter (nameMatches fact) db
      terms = map factTerms matchingFacts
   in map (Map.fromList . zip (factTerms fact)) terms

conjunct :: [[Map Term Term]] -> [Map Term Term]
conjunct = \case
  [bodyAttrs] -> bodyAttrs
  [attr1, attr2] -> -- TODO handle more than 2 clauses
    [Map.union a2 a1 | a1 <- attr1, a2 <- attr2, hasCommonValue a1 a2]

ruleAttrs :: Fact -> Map Term Term -> [Term]
ruleAttrs relation factAttrs =
  mapMaybe (flip Map.lookup factAttrs) $ factTerms relation

runRecursive :: [Rule] -> Query -> Database -> Database
runRecursive rs q =
  let deriveExtraFacts = generateKnowledgeBase evaluateLogicalOperatorsInRule rs
      f rec db =
        let db' = deriveExtraFacts db
        in if db' == db
              then db'
              else rec db'
   in filter (matches q) . fix f

main :: IO ()
main = do
  let results = runRecursive rules query database
  traverse_ print results


-- Datalog setup code:
x, y, z :: Term
x = Var "X"
y = Var "Y"
z = Var "Z"

parent :: Term -> Term -> Fact
parent p c = Fact "parent" [p, c]

ancestor :: Term -> Term -> Fact
ancestor a b = Fact "ancestor" [a, b]

personA, personB, personC, personD, personAA, personBB, personCC :: Term
personA = Constant "A"
personB = Constant "B"
personC = Constant "C"
personD = Constant "D"
personAA = Constant "AA"
personBB = Constant "BB"
personCC = Constant "CC"

database :: Database
database =
  [ parent personA personB
  , parent personB personC
  , parent personC personD
  , parent personAA personBB
  , parent personBB personCC
  ]


rules :: [Rule]
rules = [ancestorBaseRule, ancestorRecursiveRule]

ancestorBaseRule, ancestorRecursiveRule :: Rule
ancestorBaseRule = Rule hd body where
  hd = ancestor x y
  body = [parent x y]
ancestorRecursiveRule = Rule hd body where
  hd = ancestor x z
  body = [parent x y, ancestor y z]

query :: Query
query = ancestor x y
