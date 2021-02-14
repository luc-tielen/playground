
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main ( main ) where

import Data.Foldable (traverse_)
import Data.Function ((&))
import Control.Arrow ((>>>))

type Variable = String

type Name = String

data Term
  = Constant String
  | Var Variable
  deriving (Eq, Show)

data Fact
  = Fact
  { factName :: Name
  , factTerms :: [Term]
  } deriving (Show)

data Rule = Rule Fact [Fact]

type Database = [Fact]

type Query = Fact



-- Facts
x :: Term
x = Var "X"

parent :: Term -> Term -> Fact
parent p c = Fact "parent" $ [p, c]

abe = Constant "Abe"
bob = Constant "Bob"
abby = Constant "Abby"
carl = Constant "Carl"
connor = Constant "Connor"
beatrice = Constant "beatrice"

database :: Database
database =
  [ parent abe bob
  , parent abby bob
  , parent bob carl
  , parent bob connor
  , parent beatrice carl
  ]


-- Rules
rules :: [Rule]
rules = []

-- runSimplest :: Database -> [Rule] -> Query -> [Fact]
-- runSimplest db _ q = filter (\f -> factName f == factName q) db

runWithFilter :: Database -> [Rule] -> Query -> [Fact]
runWithFilter db _ q = filter (matches q) db

matches :: Fact -> Fact -> Bool
matches q fact = namesMatch && attributesMatch where
  namesMatch = factName fact == factName q
  attributesMatch =
    zip (factTerms q) (factTerms fact)
    & filter (fst >>> isConstant)
    & all (uncurry (==))
  isConstant Constant{} = True
  isConstant _ = False


query, query' :: Query
query = parent x carl
query' = parent bob x

main :: IO ()
main = do
  let results = runWithFilter database rules query
  let results' = runWithFilter database rules query'
  print (matches (parent x bob) (parent abby bob) )
  print (matches (parent abby x) (parent abby bob) )
  print (matches (parent x bob) (parent abby carl) )
  traverse_ print results
  traverse_ print results'
