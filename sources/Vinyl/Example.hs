module Vinyl.Example where
import Vinyl.Extra

{-|
@
stack build && stack exec -- vinyl-generics-example
@
-}
main :: IO ()
main = do
 putStrLn ""
 traverse_ print "Vinyl"
