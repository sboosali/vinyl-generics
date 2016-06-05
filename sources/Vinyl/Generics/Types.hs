module Vinyl.Generics.Types where

import Data.Vinyl

type Record = Rec ElField

-- newtype Record (as :: [(Symbol,*)]) = Record { getRecord ::
--  Rec ElField as
--  }

-- newtype Record (f :: * -> *) (as :: [(Symbol,*)]) = Record { getRecord ::
--  Rec (f :. ElField) as
--  }
