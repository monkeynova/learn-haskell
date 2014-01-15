module Main where

import TAP

main = do
       pass $ Just "Foo"
       pass Nothing
       is 1 1 Nothing
       done_testing
