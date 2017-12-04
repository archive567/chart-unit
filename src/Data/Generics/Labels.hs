{-# LANGUAGE CPP, TypeFamilies, TypeApplications,ScopedTypeVariables
    , FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Generics.Labels
  () where

import Protolude
import Data.Generics.Product
import GHC.OverloadedLabels

{-
example:
data Foo = Foo {bar :: Int} deriving (Generic)
_bar :: Lens' Foo Int
_bar = #bar
-}

{-
This is the instance head that seems to give the best type inference.
-}
instance (HasField x a s, Functor f, lens ~ ((a -> f a) -> s -> f s)) =>
  IsLabel x lens where
#if __GLASGOW_HASKELL__ >= 802
  fromLabel = field @ x
#else
  fromLabel _ = field @ x
#endif
