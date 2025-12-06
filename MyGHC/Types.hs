{-# LANGUAGE NoImplicitPrelude #-}

module MyGHC.Types where

import qualified GHC.Types as GHC (Char)
import GHC.Show (Show(..))

type Char = GHC.Char

type String = [Char]

data Bool = False | True

data Ordering = LT | EQ | GT