module Types
    ( getXA
    , getXB
    , getSingletonSet
    , makeSet
    , makeCategorySet
    , CategoryX
    , CategorySet
    , Set
    ) where

data CategoryX = A
               | B
                 deriving Show


data Set s = Set s
           | SingletonSet
           | FamilyOfSets [Set s]
             deriving Show

data CategorySet s = CategorySet s
                     deriving Show

getXA = A
getXB = B
getSingletonSet = SingletonSet
makeSet = Set
makeCategorySet = CategorySet
