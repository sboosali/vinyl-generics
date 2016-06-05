module Vinyl.Generics.Example where
-- import Vinyl.Generics

import Data.Foldable (traverse_)

{-|
@
stack build && stack exec -- example-vinyl-generics
@
-}
main :: IO ()
main = do
 putStrLn ""
 traverse_ print "Vinyl"
