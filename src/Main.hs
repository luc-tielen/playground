
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}  -- TODO remove
module Main ( main ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Foldable (traverse_)
import Data.Function ((&))
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
  }

type Database = [Fact]

type Query = Fact



-- Facts
x, y :: Term
x = Var "X"
y = Var "Y"

parent :: Term -> Term -> Fact
parent p c = Fact "parent" [p, c]

father :: Term -> Term -> Fact
father a b = Fact "father" [a, b]

man, woman, animal, human :: Term -> Fact
man a = Fact "man" [a]
woman a = Fact "woman" [a]
animal a = Fact "animal" [a]
human a = Fact "human" [a]

fatherRule :: Rule
fatherRule = Rule hd body where
  hd = father x y
  body = [parent x y, man x]

abe, bob, abby, carl, connor, tiger :: Term
abe = Constant "Abe"
bob = Constant "Bob"
abby = Constant "Abby"
carl = Constant "Carl"
connor = Constant "Connor"
beatrice = Constant "Beatrice"
tiger = Constant "Tiger"

database :: Database
database =
  [ parent abe bob
  , parent abby bob
  , parent bob carl
  , parent bob connor
  , parent beatrice carl
  , man abe
  , man bob
  , woman abby
  , woman beatrice
  ]


-- Rules
rules :: [Rule]
rules = [fatherRule]

generateKnowledgeBase :: (Database -> Rule -> [Fact])
                      -> [Rule] -> Database -> Database
generateKnowledgeBase f rs db = nubOrd $ concatMap (f db) rs

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

runLogicalOperators :: Database -> [Rule] -> Query -> [Fact]
runLogicalOperators db rs q =
  let kb = generateKnowledgeBase evaluateLogicalOperatorsInRule rs db
   in filter (matches q) kb

-- in blog: called match_relation_and_db
matchRelationInDb :: Database -> Fact -> [Map Term Term]
matchRelationInDb db fact =
  let matchingFacts = filter (nameMatches fact) db
      terms = map factTerms matchingFacts
   in map (Map.fromList . zip (factTerms fact)) terms

conjunct :: [[Map Term Term]] -> [Map Term Term]
conjunct = \case
  [bodyAttrs] -> bodyAttrs
  [attr1, attr2] ->
    [Map.union a2 a1 | a1 <- attr1, a2 <- attr2, hasCommonValue a1 a2]

ruleAttrs :: Fact -> Map Term Term -> [Term]
ruleAttrs relation factAttrs =
  mapMaybe (flip Map.lookup factAttrs) $ factTerms relation

query :: Query
query = father x y

main :: IO ()
main = do
  let results = runLogicalOperators database rules query
  traverse_ print results
