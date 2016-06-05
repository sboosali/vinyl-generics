{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns, RecordWildCards #-}

{-|

-}
module Vinyl.Generics.Example where
import Vinyl.Generics

import Data.Vinyl (Rec(..),(<+>),rtraverse)
import Data.Vinyl.Lens (RSubset(..))
import Data.Vinyl.Functor (Identity(..),Compose(..),(:.))

import Control.Arrow ((>>>))
import Data.Foldable (traverse_)
import Control.Concurrent.STM (STM,TVar,newTVar,readTVar, writeTVar,atomically,)

--------------------------------------------------------------------------------

{-|
@
stack build && stack exec -- example-vinyl-generics
@
-}
main :: IO ()
main = do
  mainSimple
  mainVariables

mainSimple = do
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

  -- putStrLn ""

--old rreplace (Identity (intoProduct p) :& RNil) f_generic

mainVariables = do
  putStrLn ""

  --
  vEnvironment@(vFlag :& vCounter :& _) <- atomically $ newDynamicEnvironment (Environment False 0)
  Environment{..} <- atomically $ readDynamicEnvironment vEnvironment
  print eFlag
  print eCounter

  putStrLn ""
  atomically $ writeTVar vFlag True
  atomically $ writeTVar vCounter 1
  e <- atomically $ readDynamicEnvironment vEnvironment
  print e

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

{-| a snapshot of our environment.

we can add fields to this type without changing the definitions of:

* 'DynamicEnvironment'
* 'readDynamicEnvironment'
* 'newDynamicEnvironment'

We only need to add a setter (using 'rget' and 'writeTVar'), wherever needed.

-}
data Environment = Environment
 { eFlag    :: Bool
 , eCounter :: Integer
 } deriving (Show,Generic)
instance IsProduct Environment

{-| An immutable product of mutable variables.

-}
type DynamicEnvironment = Rec TVar (Fields Environment)

{-|
@= @
-}
newDynamicEnvironment :: Environment -> STM DynamicEnvironment
newDynamicEnvironment (intoProduct -> environment)
   = rtraverse _newTVar environment
  where
  _newTVar :: Identity a -> STM (TVar a)
  _newTVar = getIdentity >>> newTVar

{-old

_newTVar :: Identity a -> (STM :. TVar) a
_newTVar = getIdentity >>> newTVar >>> Compose

-}

{-|
@= @
-}
readDynamicEnvironment :: DynamicEnvironment -> STM Environment
readDynamicEnvironment
    = rtraverse (readTVar >>> fmap Identity)
  >>> fmap fromProduct

--------------------------------------------------------------------------------
