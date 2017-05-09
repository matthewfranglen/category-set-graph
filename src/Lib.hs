module Lib
    ( someFunc
    , toSingleton
    ) where

import Types

toSingleton :: Types.CategoryX -> Types.CategorySet (Types.Set s v)
toSingleton _ = Types.makeCategorySet $ Types.getSingletonSet

someFunc :: IO ()
someFunc = putStrLn "someFunc"
