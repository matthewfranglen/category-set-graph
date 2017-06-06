module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data CatA = AA
          | AB
            deriving (Eq, Show)

data CatB = BA
          | BB
            deriving (Eq, Show)

f :: CatA -> CatA
f AA = AB
f AB = AA

g :: CatB -> CatB
g BA = BB
g BB = BA

functor :: CatA -> CatB
functor AA = BA
functor AB = BB

functor' :: (CatA -> CatA) -> (CatB -> CatB)
functor' f = functor . f . inverse
    where inverse :: CatB -> CatA
          inverse BA = AA
          inverse BB = AB
