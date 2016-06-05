{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

{-|

-}
module Vinyl.Generics.Example where
import Vinyl.Generics

import Data.Vinyl (Rec(..),(<+>))
import Data.Vinyl.Lens (RSubset(..))
import Data.Vinyl.Functor (Identity(..))

import Data.Foldable (traverse_)

--------------------------------------------------------------------------------

{-|
@
stack build && stack exec -- example-vinyl-generics
@
-}
main :: IO ()
main = do
  putStrLn ""

  let p = P False 0 ""
  print $ p
  print $ intoProduct p

  putStrLn ""

  let f_original = F p ()
  let f_generic = intoProduct f_original

  -- "inline" the products, partly
  let f_inlinePartly = case f_generic of
                (Identity p' :& r) -> Identity (intoProduct p') :& r
  -- "inline" the products, totally
  let f_inlineTotally = case f_inlinePartly of
                (Identity p'' :& r) -> p'' <+> r
                --TODO cleanup: type changing lens, rmodify.

  print $ f_original
  print $ f_generic
  print $ f_inlinePartly
  print $ f_inlineTotally
  putStrLn ""

--old rreplace (Identity (intoProduct p) :& RNil) f_generic

--------------------------------------------------------------------------------

{-| A product type.

with @-XStandaloneDeriving@:

@
instance 'IsProduct' P
@

-}
data P = P
 { pB :: Bool
 , pI :: Integer
 , pS :: String
 } deriving (Show,Generic)
instance IsProduct P

{-old

deriving (Show,Generic,IsProduct)
--TODO with DeriveAnyClass, eppects fields to instantiate IsProduct

-}

{-| A product type. Polymorphic.

with @-XStandaloneDeriving@:

@
instance 'IsProduct' (F a)
@

-}
data F a = F
  { fp :: P
  , fA :: a
  } deriving (Show,Generic)
instance IsProduct (F a)

--------------------------------------------------------------------------------
