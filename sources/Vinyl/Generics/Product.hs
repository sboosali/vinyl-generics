{-# LANGUAGE RankNTypes, ScopedTypeVariables, DefaultSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, LambdaCase #-}
{-# LANGUAGE TypeFamilies, ExplicitNamespaces, DataKinds, UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

{-|
-}
module Vinyl.Generics.Product where
import Vinyl.Generics.Extra
import Vinyl.Generics.Types

import Data.Vinyl hiding (Dict)
import Data.Vinyl.TypeLevel
import Data.Vinyl.Functor (Identity(..))
--(Rec(..),(<+>))
import Data.Constraint (Dict(..),(:-)(..))

import Data.Proxy
import GHC.Generics


rcastPair :: [ Z, 'S Z ] ~ RImage [a,b] [a,b]
          => Rec f '[a,b] -> Rec f '[a,b]
rcastPair = rcast

rcastLeft :: Rec f (as ++ bs) -> Rec f as
rcastLeft = undefined -- rcast

rcastRight :: Rec f (as ++ bs) -> Rec f bs
rcastRight = undefined -- rcast

{-old

rcastLeft :: Rec f (as ++ bs) -> Rec f as
rcastLeft = rcast

rcastLeft :: (cs ~ (as ++ bs)) => Rec f cs -> Rec f as

rcast4 :: Rec f ([a,b] ++ [c,d]) -> Rec f [a,b]
rcast4 = rcast

https://www.reddit.com/r/haskell/comments/4mt8kg/help_could_not_deduce_as_as_bs_with_vinyl/

-}


--------------------------------------------------------------------------------

{-| a generic representation of the datatype.

a tagged ('ElField') product ('Rec').

-}
type ProductOf a = Rec I (Fields a)
-- type ProductOf a = FieldCoRec FieldRec (Algebra a)

{-| A product type is a heterogeneous sequence of fields. for example:

@
TODO
@

-}
class IsProduct a where
  type Fields a :: [*]
  type Fields a = GFields (Rep a)

  intoProduct :: a -> ProductOf a
  fromProduct :: ProductOf a -> a

  default intoProduct
   :: (Generic a, GIsProduct (Rep a), Fields a ~ GFields (Rep a))
   => a -> ProductOf a
  intoProduct = from >>> intoGProduct

  default fromProduct
   :: (Generic a, GIsProduct (Rep a), Fields a ~ GFields (Rep a))
   => ProductOf a -> a
  fromProduct = fromGProduct >>> to

--------------------------------------------------------------------------------

{-|

-}
type GProductOf f = Rec I (GFields f)

{- | "Generic Product".

'Product' lifted to unary type constructors to accomodate 'Generic'.

non-instances: 'V1' and ':+:'

@
'GFields' (Rep a) ~ 'Fields' a
@

-}
class GIsProduct f where
  type GFields f :: [*]
  intoGProduct :: f x -> GProductOf f
  fromGProduct :: GProductOf f -> f x

-- | use 'RNil' (@'Rec' f '[]@)
instance GIsProduct U1 where
 type GFields U1 = '[]
 intoGProduct U1 = RNil
 fromGProduct RNil = U1
 -- TODO Spurious nonexhaustive patterns warning

-- | use 'Identity' (a singleton record)
instance GIsProduct (K1 R a) where --TODO R?
 type GFields (K1 R a) = '[a]
 intoGProduct (K1 a) = Identity a :& RNil
 fromGProduct (Identity a :& RNil) = K1 a
 -- TODO Spurious nonexhaustive patterns warning

-- | use '<+>'
instance
 ( GIsProduct f, GIsProduct g
 -- , GFields f <: GFields f
 -- , GFields g <: GFields g
 )
 => GIsProduct (f :*: g) where
 type GFields (f :*: g) = GFields f ++ GFields g
 intoGProduct (f :*: g) = intoGProduct f <+> intoGProduct g
 fromGProduct p = undefined -- (fromGProduct (rcast p)) :*: (fromGProduct (rcast p)) --TODO duplicates?

  -- where
  -- pFs = Proxy :: Proxy (GFields f)
  -- pGs = Proxy :: Proxy (GFields g)

{-old

#1

fromGProduct p = (fromGProduct (rcast p)) :*: (fromGProduct (rcast p)) --TODO duplicates?

Could not deduce (RSubset
                    (GFields f)
                    (GFields f ++ GFields g)
                    (RImage (GFields f) (GFields f ++ GFields g)))

Could not deduce (RSubset
                    (GFields g)
                    (GFields f ++ GFields g)
                    (RImage (GFields g) (GFields f ++ GFields g)))

lol, must prove those facts.

#2

instance
 ( (GFields f) <: (GFields f ++ GFields g), (GFields g) <: (GFields f ++ GFields g)

No instance for (Data.Vinyl.Lens.RElem
                    a '[P, a] (Data.Vinyl.TypeLevel.RIndex a '[P, a]))
   arising from a use of ‘Vinyl.Generics.Product.$gdmintoProduct’
 In the expression: Vinyl.Generics.Product.$gdmintoProduct
 In an equation for ‘intoProduct’:
     intoProduct = Vinyl.Generics.Product.$gdmintoProduct
 In the instance declaration for ‘IsProduct (F a)’

...

pushes the proof to the client, destroying derivability

-}

-- | (ignore metadata)
instance (GIsProduct f) => GIsProduct (M1 i t f) where
 type GFields (M1 i t f) = GFields f
 intoGProduct (M1 f) = intoGProduct f
 fromGProduct f = M1 (fromGProduct f)

{-old

-- | use 'IsProduct'
instance (IsProduct a) => GIsProduct (K1 R a) where --TODO R?
 type GFields (K1 R a) = Fields a
 intoGProduct (K1 a) = intoProduct a
 fromGProduct a = K1 (fromProduct a)

this would in line all the fields.
Perhaps useful for Reading unfamiliar programs, Could be a separate instance.
 still needs Base case I think, i.e. Identity

-}

--
-- -- | selector metadata.
-- instance (GIsProduct f) => GIsProduct (M1 S t f) where
--  type GFields (M1 S t f) = []
--  intoGProduct =
--  fromGProduct =
--
-- -- | constructor metadata.
-- instance (GIsProduct f) => GIsProduct (M1 C t f) where
--  type GFields (M1 C t f) = []
--  intoGProduct =
--  fromGProduct =
--
-- -- | datatype metadata.
-- instance (GIsProduct f) => GIsProduct (M1 D t f) where
--  type GFields (M1 D t f) = []
--  intoGProduct =
--  fromGProduct =

--------------------------------------------------------------------------------
