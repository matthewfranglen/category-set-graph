module Types
    ( getXA
    , getXB
    , getSingletonSet
    , makeSet
    , makeCategorySet
    ) where

data CategoryX = A
               | B
                 deriving Show


data Set s v = Set s v
             | SingletonSet
               deriving Show

data CategorySet s = CategorySet s

getXA = A
getXB = B
getSingletonSet = SingletonSet
makeSet = Set
makeCategorySet = CategorySet
