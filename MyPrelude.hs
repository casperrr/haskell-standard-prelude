{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use /=" #-}
{-# HLINT ignore "Use ==" #-}

module MyPrelude where

import GHC.Show (Show(..))
import qualified GHC.Types as GHC

-----------------------------------------------------
-- Types
-----------------------------------------------------

type Char = GHC.Char
type String = [Char]
type Int = GHC.Int

------------------------------
-- Bool
------------------------------

data Bool = True | False
    -- deriving (Show)

--------- Bool Functions ---------

not :: Bool -> Bool
not True  = False
not False = True

otherwise :: Bool
otherwise = True

infixr 3 &&
infixr 2 ||

(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t _ = t
ifThenElse False _ f = f

--------- Bool Instances ---------

instance Eq Bool where
    (==) :: Bool -> Bool -> Bool
    True  == True  = True
    False == False = True
    _     == _     = False

instance Show Bool where
    show :: Bool -> String
    show True  = "True"
    show False = "False"

instance Ord Bool where
    compare :: Bool -> Bool -> Ordering
    compare False False = EQ
    compare False True  = LT
    compare True  False = GT
    compare True  True  = EQ

------------------------------
-- Ordering
------------------------------

data Ordering = LT | EQ | GT
    deriving (Show)

--------- Ordering Instances ---------

instance Eq Ordering where
    (==) :: Ordering -> Ordering -> Bool
    LT == LT = True
    EQ == EQ = True
    GT == GT = True
    _  == _  = False

------------------------------
-- Ordering
------------------------------

data Maybe a = Nothing | Just a

--------- Maybe Instances ---------
instance Eq a => Eq (Maybe a) where
    (==) :: Maybe a -> Maybe a -> Bool
    Nothing == Nothing = True
    Just x  == Just y  = x == y
    _       == _       = False



-----------------------------------------------------
-- Classes
-----------------------------------------------------

--------- Eq Class ---------
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x /= y = not (x == y)
    x == y = not (x /= y)

-------- Ord Class ---------
class Eq a => Ord a where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a

    compare x y = case x == y of
        True  -> EQ
        False -> case x <= y of
            True  -> LT
            False -> GT

    x <= y = case compare x y of { GT -> False; _ -> True }
    x >= y = y <= x
    x >  y = not (x <= y)
    x <  y = not (y <= x)

    max x y = if x <= y then y else x
    min x y = if x <= y then x else y

--------- Num Class ---------
class Num a where
    (+), (-), (*)       :: a -> a -> a
    negate, abs, signum :: a -> a

    x - y = x + negate y