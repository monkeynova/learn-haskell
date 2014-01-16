module Main where

import TAP

main = do
       run_tests ( do
                   TAP.pass $ Just "Bar"
                   TAP.pass Nothing
                   TAP.fail $ Just "test failure"
                   is 1 1 Nothing
                   is 1 2 Nothing
                 )
