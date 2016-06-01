module Vinyl.Generics.Example where
import Vinyl.Generics.Extra

{-|
@
stack build && stack exec -- vinyl-generics-example
@
-}
main :: IO ()
main = do
 putStrLn ""
 traverse_ print "Vinyl"
