import Test.QuickCheck

import Lib

main :: IO ()
main = quickCheck prop_functor

prop_functor :: CatA -> Bool
prop_functor x = (functor . f) x == (g . functor) x

instance Arbitrary CatA where
    arbitrary = elements [AA, AB]

instance Arbitrary CatB where
    arbitrary = elements [BA, BB]
