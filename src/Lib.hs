module Lib
    ( someFunc
--  , toNothing
    , toSingleton
    ) where

import Types

-- With the change in data types this is no longer legal
-- toNothing :: Types.CategoryX -> Types.CategorySet (Types.Set s)
-- toNothing _ = Types.makeCategorySet Void

toSingleton :: Types.CategoryX -> Types.CategorySet (Types.Set s)
toSingleton _ = Types.makeCategorySet $ Types.getSingletonSet

someFunc :: IO ()
someFunc = putStrLn "someFunc"
