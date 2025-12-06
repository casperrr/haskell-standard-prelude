{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use /=" #-}
{-# HLINT ignore "Use ==" #-}

module MyPrelude where

import GHC.Show (Show(..))
import qualified GHC.Types as GHC
import GHC.Types (Int)
import GHC.Num (Num(..), (+), (-), (*), fromInteger, Integer)
import qualified GHC.Classes as GHC
import GHC.Err (error)

default (Int, Integer)

-----------------------------------------------------
-- Basic Generic Functions
-----------------------------------------------------

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

infixr 9 .
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)

infixr 0 $
($) :: (a -> b) -> a -> b
f $ x = f x

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

-----------------------------------------------------
-- Types
-----------------------------------------------------

type Char = GHC.Char
type String = [Char]
-- type Int = GHC.Int

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
-- Maybe
------------------------------
data Maybe a = Nothing | Just a

--------- Maybe Instances ---------
instance Eq a => Eq (Maybe a) where
    (==) :: Maybe a -> Maybe a -> Bool
    Nothing == Nothing = True
    Just x  == Just y  = x == y
    _       == _       = False

instance Ord a => Ord (Maybe a) where
    compare :: Maybe a -> Maybe a -> Ordering
    compare Nothing Nothing   = EQ
    compare Nothing (Just _)  = LT
    compare (Just _) Nothing  = GT
    compare (Just x) (Just y) = compare x y

------------------------------
-- List
------------------------------
-- data [] a = [] | a : [a]

--------- List Instances ---------
instance Eq a => Eq [a] where
    (==) :: [a] -> [a] -> Bool
    []     == []     = True
    (x:xs) == (y:ys) = (x == y) && (xs == ys)
    _      == _      = False

instance Ord a => Ord [a] where
    compare :: [a] -> [a] -> Ordering
    compare []     []     = EQ
    compare []     (_:_)  = LT
    compare (_:_)  []     = GT
    compare (x:xs) (y:ys) = case compare x y of
        EQ  -> compare xs ys
        ord -> ord

---------- List Functions ---------

infixr 5 ++
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

head :: [a] -> a
head (x:_) = x
head []    = error "head: empty list"

last :: [a] -> a
last = head . reverse

tail :: [a] -> [a]
tail (_:xs) = xs
tail []     = error "tail: empty list"

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs

singleton :: a -> [a]
singleton x = [x]

init :: [a] -> [a]
init = reverse . tail . reverse

null :: [a] -> Bool
null [] = True
null _  = False

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

intersperse :: a -> [a] -> [a]
intersperse _ []     = []
intersperse _ [y]    = [y]
intersperse x (y:ys) = y : x : intersperse x ys

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 _ []     = error "foldl1: empty list"
foldl1 f (x:xs) = foldl f x xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

concat :: [[a]] -> [a]
concat = foldr (++) []

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _ z []     = [z]
scanl f z (x:xs) = z : scanl f (f z x) xs

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ z [] = [z]
scanr f z (x:xs) = f x q : qs
    where qs@(q:_) = scanr f z xs

take :: Int -> [a] -> [a]
take n xs = case n == 0 of
    True -> []
    False -> case_take n xs
  where
    case_take _ [] = []
    case_take m (x:xs) = x : take (m-1) xs

drop :: Int -> [a] -> [a]
drop n xs = reverse . take (length xs - n) $ reverse xs

zip :: [a] -> [b] -> [(a,b)]
zip []     _      = []
zip _      []     = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

------------------------------
-- Int
------------------------------
-- type Int = GHC.Int
--------- Int Instances ---------
instance Eq Int where
    (==) :: Int -> Int -> Bool
    x == y = case x GHC.== y of
        GHC.True  -> True
        GHC.False -> False

instance Ord Int where
    compare :: Int -> Int -> Ordering
    compare x y = case x GHC.== y of
        GHC.True  -> EQ
        GHC.False -> case x GHC.<= y of
            GHC.True  -> LT
            GHC.False -> GT
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
-- Too complicated to reimplement here
-- class Num a where
--     (+), (-), (*)       :: a -> a -> a
--     negate, abs, signum :: a -> a
--     fromInteger         :: Integer -> a

--     x - y = x + negate y
--     negate x = 0 - x