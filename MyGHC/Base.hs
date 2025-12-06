{-# LANGUAGE NoImplicitPrelude #-}

module MyGHC.Base where

import MyGHC.Types -- ( Bool(..), Ordering(..), Char )
import MyGHC.Classes
import GHC.Show (Show(..))

-------------------------------------
-- Bool instances
-------------------------------------

instance Eq Bool where
    (==) :: Bool -> Bool -> Bool
    True  == True  = True
    False == False = False
    _     == _     = False

instance Ord Bool where
    compare :: Bool -> Bool -> Ordering
    compare False False = EQ
    compare False True  = LT
    compare True  False = GT
    compare True  True  = EQ


instance Show Bool where
    show :: Bool -> String
    show True  = "True"
    show False = "False"