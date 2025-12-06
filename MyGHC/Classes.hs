{-# LANGUAGE NoImplicitPrelude #-}

module MyGHC.Classes where

import MyGHC.Types
-- import MyGHC.Base
-- import MyData.Bool

------------------------------
-- Important Funcs
------------------------------

not :: Bool -> Bool
not True = False
not False = True

otherwise :: Bool
otherwise = True

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x /= y = not (x == y)
    x == y = not (x /= y)

class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    min, max :: a -> a -> a

    compare x y
      | x == y = EQ
      | x <= y = LT
      | otherwise = GT

    x <= y = case compare x y of { GT -> False; _ -> True }
    x >= y = y <= x
    x > y = not (x <= y)
    x < y = not (y <= x)

    max x y = if x <= y then y else x
    min x y = if x <= y then x else y
    -- {-# MINIMAL compare | (<=) #-}