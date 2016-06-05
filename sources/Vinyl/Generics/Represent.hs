{-# LANGUAGE RankNTypes, ScopedTypeVariables, DefaultSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, LambdaCase #-}
{-# LANGUAGE TypeFamilies, ExplicitNamespaces, DataKinds, UndecidableInstances #-}

{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

{-|

Background:

* <http://ryanglscott.github.io/2016/05/12/whats-new-with-ghc-generics-in-80/>

Alternatives:

* <http://hackage.haskell.org/package/generics-sop>;
more mature; rolls its own records / co-records;
doesn't support type-level labels.
* <http://hackage.haskell.org/package/generics-eot>;
"either of tuples" is simpler;
but nested, which is awkward.

-}
module Vinyl.Generics.Represent where
-- import Vinyl.Generics.Extra
-- import Vinyl.Generics.Types
-- import Vinyl.Generics.Product
-- import Vinyl.Generics.Sum

-- import Data.Vinyl (Rec(..))
--
-- import GHC.Generics
--

--------------------------------------------------------------------------------
--
-- {-| a generic representation of the datatype.
--
-- a tagged ('ElField') sum ('CoRec') of tagged ('ElField') products ('Rec').
--
-- -}
-- type Representation a = CoRec (Rec I) (CasesAndFields a)
-- -- type Representation a = FieldCoRec FieldRec (Algebra a)
--
-- {-|
--
-- an "algebraic datatype" is a "sum of products". for example:
--
-- @
-- TODO
-- @
--
-- -}
-- class Represent a where
--   type CasesAndFields a :: [[*]] -- TODO naming
--
--   intoRepresentation :: a -> Representation a
--   fromRepresentation :: Representation a -> a
--
--   type CasesAndFields a = GCasesAndFields(Rep a)
--
--   default intoRepresent :: (Generic a, GCasesAndFields (Rep a)) => a -> Representation a
--   intoRepresent = intoGRepresentation . from
--
--   default fromRepresent :: (Generic a, GCasesAndFields (Rep a)) => Representation a -> a
--   fromRepresent = to . fromGRepresentation
--
-- --------------------------------------------------------------------------------
--
-- type GRepresentation f x = CoRec (Rec I) (GCasesAndFields (f x))
--
-- {- | "Generic Represent".
--
-- 'Represent' lifted to unary type constructors to accomodate 'Generic'.
--
-- -}
-- class GRepresent f where
--   type GCasesAndFields f :: [[*]] -- TODO naming
--   intoGRepresentation :: f x -> GRepresentation f x
--   fromGRepresentation :: GRepresentation f x -> f x
--
-- -- | @'CoRec' f '[]@
-- instance GRepresent V1 where
--  type GCasesAndFields V1 = '[]
--  intoGRepresentation =
--  fromGRepresentationation =
--
-- -- | @'Rec' f '[]@
-- instance GRepresent U1 where
--  type GCasesAndFields U1 = '['[]]
--  intoGRepresentation =
--  fromGRepresentation =
--
-- -- | call 'Represent'
-- instance (Represent a) => GRepresent (K1 R a) where
--  type GCasesAndFields (K1 R a) = '['[]]
--  intoGRepresentation =
--  fromGRepresentation =
--
-- -- | 'CoRec' cases.
-- instance (GRepresent (f), GRepresent (g)) => GRepresent (f :+: g) where
--  type GCasesAndFields (f :+: g) = '['[]]
--  intoGRepresentation =
--  fromGRepresentation =
--
-- -- | 'Rec' fields.
-- instance (GRepresent (f), GRepresent (g)) => GRepresent (f :*: g) where
--  type GCasesAndFields (f :*: g) = '['[]]
--  intoGRepresentation =
--  fromGRepresentation =
--
-- -- | selector metadata.
-- instance (GRepresent (f)) => GRepresent (M1 S t f) where
--  type GCasesAndFields (M1 S t f) = '['[]]
--  intoGRepresentation =
--  fromGRepresentation =
--
-- -- | constructor metadata.
-- instance (GRepresent (f)) => GRepresent (M1 C t f) where
--  type GCasesAndFields (M1 C t f) = '['[]]
--  intoGRepresentation =
--  fromGRepresentation =
--
-- -- | datatype metadata.
-- instance (GRepresent (f)) => GRepresent (M1 D t f) where
--  type GCasesAndFields (M1 D t f) = '['[]]
--  intoGRepresentation =
--  fromGRepresentation =
--
-- --------------------------------------------------------------------------------
