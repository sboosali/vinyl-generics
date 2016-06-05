{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

{-|

-}
module Vinyl.Generics.Epample where
import Vinyl.Generics

import Data.Foldable (traverse_)

--------------------------------------------------------------------------------

{-|
@
stack build && stack epec -- epample-vinyl-generics
@
-}
main :: IO ()
main = do
  putStrLn ""

  let p = P False 0 ""
  print $ p
  print $ intoProduct p

  putStrLn ""

  let f = F p ()
  print $ f
  print $ intoProduct f

  putStrLn ""

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
