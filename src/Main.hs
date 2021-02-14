
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main ( main ) where

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
x :: Term
x = Var "X"

man, animal, human :: Term -> Fact
man a = Fact "man" [a]
animal a = Fact "animal" [a]
human a = Fact "human" [a]

humanRule :: Rule
humanRule = Rule hd body where
  hd = human x
  body = [man x]

abe, bob, tiger :: Term
abe = Constant "Abe"
bob = Constant "Bob"
tiger = Constant "Tiger"

database :: Database
database =
  [ man abe
  , man bob
  , animal tiger
  ]


-- Rules
rules :: [Rule]
rules = [humanRule]

evaluateRuleSimple :: Database -> Rule -> [Fact]
evaluateRuleSimple db r =
  let firstAtom = head $ ruleBody r
      matchingFacts = filter (nameMatches firstAtom) db
      ruleName = factName $ ruleHead r
      inferFact = Fact ruleName . factTerms
   in map inferFact matchingFacts

generateKnowledgeBase :: (Database -> Rule -> [Fact])
                      -> [Rule] -> Database -> Database
generateKnowledgeBase f rs db = nubOrd $ concatMap (f db) rs

runRuleSimple :: Database -> [Rule] -> Query -> [Fact]
runRuleSimple db rs q =
  let kb = generateKnowledgeBase evaluateRuleSimple rs db
   in filter (matches q) kb

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


query :: Query
query = human x

main :: IO ()
main = do
  let results = runRuleSimple database rules query
  -- print (matches (parent x bob) (parent abby bob) )
  -- print (matches (parent abby x) (parent abby bob) )
  -- print (matches (parent x bob) (parent abby carl) )
  traverse_ print results
