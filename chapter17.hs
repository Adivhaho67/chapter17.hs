{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Monoid

---------------------------------------------------------
-- HC17T1: Severity Data Type and Semigroup Instance
---------------------------------------------------------
data Severity = Low | Medium | High | Critical
  deriving (Eq, Show, Ord, Enum, Bounded)

instance Semigroup Severity where
    s1 <> s2 = max s1 s2

---------------------------------------------------------
-- HC17T2: Min and Max Newtypes with Semigroup
---------------------------------------------------------
newtype Min a = Min { getMin :: a } deriving (Eq, Show, Ord)
newtype Max a = Max { getMax :: a } deriving (Eq, Show, Ord)

instance Ord a => Semigroup (Min a) where
    Min x <> Min y = Min (min x y)

instance Ord a => Semigroup (Max a) where
    Max x <> Max y = Max (max x y)

---------------------------------------------------------
-- HC17T3: Monoid Instance for Severity
---------------------------------------------------------
instance Monoid Severity where
    mempty = Low
    mappend = (<>)

---------------------------------------------------------
-- HC17T4: Monoid Instance for Sum Newtype
---------------------------------------------------------
instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    mappend = (<>)

---------------------------------------------------------
-- HC17T5: combineLists Function
---------------------------------------------------------
combineLists :: [Int] -> [Int] -> [Int]
combineLists = (<>)

---------------------------------------------------------
-- HC17T6: maxSeverity Function
---------------------------------------------------------
maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat

---------------------------------------------------------
-- HC17T7: multiplyProducts Function
---------------------------------------------------------
multiplyProducts :: [Product Integer] -> Product Integer
multiplyProducts = mconcat

---------------------------------------------------------
-- HC17T8: foldWithSemigroup Function
---------------------------------------------------------
foldWithSemigroup :: Semigroup a => [a] -> a
foldWithSemigroup = foldr (<>) mempty

---------------------------------------------------------
-- HC17T9: Config Data Type and Semigroup Instance
---------------------------------------------------------
data Config = Config { loggingLevel :: Int
                     , timeout :: Int
                     , retries :: Int
                     } deriving (Show, Eq)

instance Semigroup Config where
    c1 <> c2 = Config { loggingLevel = max (loggingLevel c1) (loggingLevel c2)
                      , timeout = min (timeout c1) (timeout c2)
                      , retries = max (retries c1) (retries c2)
                      }

---------------------------------------------------------
-- HC17T10: Monoid Instance for Config
---------------------------------------------------------
instance Monoid Config where
    mempty = Config { loggingLevel = minBound
                    , timeout = maxBound
                    , retries = minBound
                    }
    mappend = (<>)

---------------------------------------------------------
-- MAIN FUNCTION: Demonstrates all HC17 Tasks
---------------------------------------------------------
main :: IO ()
main = do
    putStrLn "--- HC17T1: Severity Semigroup ---"
    print $ Medium <> High
    print $ Low <> Critical

    putStrLn "\n--- HC17T2: Min/Max Semigroups ---"
    print $ Min 10 <> Min 5
    print $ Max 10 <> Max 5

    putStrLn "\n--- HC17T3: Severity Monoid ---"
    print $ mempty <> Medium
    print $ mconcat [Low, High, Medium]

    putStrLn "\n--- HC17T4: Sum Monoid ---"
    print $ mconcat [Sum 1, Sum 2, Sum 3]

    putStrLn "\n--- HC17T5: combineLists ---"
    print $ combineLists [1,2,3] [4,5,6]

    putStrLn "\n--- HC17T6: maxSeverity ---"
    print $ maxSeverity [Low, Medium, High, Critical, Medium]

    putStrLn "\n--- HC17T7: multiplyProducts ---"
    print $ multiplyProducts [Product 2, Product 3, Product 4]

    putStrLn "\n--- HC17T8: foldWithSemigroup ---"
    print $ foldWithSemigroup [Max 1, Max 5, Max 3]
    print $ foldWithSemigroup [Min 1, Min 5, Min 3]

    putStrLn "\n--- HC17T9: Config Semigroup ---"
    let c1 = Config 2 5000 3
    let c2 = Config 5 3000 1
    print $ c1 <> c2

    putStrLn "\n--- HC17T10: Config Monoid ---"
    print $ mempty <> c1
    print $ mconcat [c1, c2]
