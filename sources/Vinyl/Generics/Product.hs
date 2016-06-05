{-# LANGUAGE RankNTypes, ScopedTypeVariables, DefaultSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, LambdaCase #-}
{-# LANGUAGE TypeFamilies, ExplicitNamespaces, DataKinds, UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

{-|
-}
module Vinyl.Generics.Product where
import Vinyl.Generics.Extra

import Data.Vinyl
import Data.Vinyl.TypeLevel
import Data.Vinyl.Functor (Identity(..))
--(Rec(..),(<+>))

import GHC.Generics

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
instance (GIsProduct f, GIsProduct g) => GIsProduct (f :*: g) where
 type GFields (f :*: g) = GFields f ++ GFields g
 intoGProduct (f :*: g) = intoGProduct f <+> intoGProduct g
 fromGProduct = undefined

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
