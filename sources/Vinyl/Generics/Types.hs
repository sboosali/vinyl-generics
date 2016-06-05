{-# LANGUAGE AllowAmbiguousTypes #-}

module Vinyl.Generics.Types where

import Data.Vinyl hiding (Dict)
import Data.Vinyl.TypeLevel
import Data.Constraint (Dict(..),(:-)(..))
import Data.Constraint.Unsafe (unsafeCoerceConstraint)

type Record = Rec ElField

-- newtype Record (as :: [(Symbol,*)]) = Record { getRecord ::
--  Rec ElField as
--  }

-- newtype Record (f :: * -> *) (as :: [(Symbol,*)]) = Record { getRecord ::
--  Rec (f :. ElField) as
--  }

-- {-|
--
-- -}
-- proof_ReflexiveSubset :: proxy as -> Dict (as ⊆ as)
-- proof_ReflexiveSubset _ = unsafeCoerce Dict
-- -- proof_ReflexiveSubset _ = unsafeCoerce Dict

{-|

-}
proof_LeftAppendPreservesSubset
 :: proxy as -> proxy bs -> proxy cs
 -> (as ⊆ bs) :- (as ⊆ (cs ++ bs))
proof_LeftAppendPreservesSubset _ _ _ = unsafeCoerceConstraint

{-|

-}
proof_RightAppendPreservesSubset
 :: proxy as -> proxy bs -> proxy cs
 ->  (as ⊆ bs) :- (as ⊆ (bs ++ cs))
proof_RightAppendPreservesSubset _ _ _ = unsafeCoerceConstraint

{-err

#1

Couldn't match type ‘bs ++ cs’ with ‘bs ++ cs0’
NB: ‘++’ is a type function, and may not be injective
The type variable ‘cs0’ is ambiguous
Expected type: (as ⊆ bs) :- (as ⊆ (bs ++ cs))
  Actual type: (as ⊆ bs) :- (as ⊆ (bs ++ cs0))
  In the ambiguity check for the type signature for ‘proof_RightAppendPreservesSubset’:

To defer the ambiguity check to use sites, enable AllowAmbiguousTypes

#2

Could not deduce (RSubset as (bs ++ cs) (RImage as (bs ++ cs)))
  arising from a use of ‘Dict’
from the context (as ⊆ bs)

-}

{- old

LANGUAGE DataKinds, KindSignatures
:: forall (as :: [*]) (bs :: [*]) (cs :: [*]).

-}
