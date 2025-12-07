{-# LANGUAGE RequiredTypeArguments #-}

module Data.Flag where

import GHC.Generics

newtype Flag (s :: symbol) = Flag Bool deriving (Eq, Ord, Show, Generic)

flag :: forall k. forall (s :: k) -> Bool -> Flag s
flag _ = Flag
