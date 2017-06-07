module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data CatA = AA
          | AB
            deriving (Eq, Show)

type FA = (CatA -> CatA)

data CatB = BA
          | BB
            deriving (Eq, Show)

type GB = (CatB -> CatB)

f :: CatA -> CatA
f AA = AB
f AB = AA

g :: CatB -> CatB
g BA = BB
g BB = BA

functor :: CatA -> CatB
functor AA = BA
functor AB = BB

functor' :: FA -> GB
functor' f = functor . f . inverse
    where inverse :: CatB -> CatA
          inverse BA = AA
          inverse BB = AB
