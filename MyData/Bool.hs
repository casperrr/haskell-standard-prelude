{-# LANGUAGE NoImplicitPrelude #-}

module MyData.Bool where

import MyGHC.Types

-- not :: Bool -> Bool
-- not True = False
-- not False = True

(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-- otherwise :: Bool
-- otherwise = True