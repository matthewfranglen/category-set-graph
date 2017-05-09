module Lib
    ( someFunc
    , toNothing
    , toSingleton
    ) where

import Types

toNothing :: Types.CategoryX -> Types.CategorySet (Types.Set s)
toNothing _ = Types.makeCategorySet []

toSingleton :: Types.CategoryX -> Types.CategorySet (Types.Set s)
toSingleton _ = Types.makeCategorySet $ [Types.getSingletonSet]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
