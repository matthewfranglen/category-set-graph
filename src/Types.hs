module Types
    ( getXA
    , getXB
    , getSingletonSet
    , makeSet
    , makeCategorySet
    ) where

data CategoryX = A
               | B

data Set s v = Set s v
             | SingletonSet

data CategorySet s = CategorySet s

getXA = A
getXB = B
getSingletonSet = SingletonSet
makeSet = Set
makeCategorySet = CategorySet
