module Main where

import Control.Monad.RWS
import TAP

main = do
       let (isOK,s,output) = subtest ( do
                                          TAP.pass $ Just "Bar"
                                          TAP.pass Nothing
                                          TAP.fail $ Just "test failure"
                                          is 1 1 Nothing
                                          is 1 2 Nothing
                                          done_testing
                                        )
       mapM_ putStrLn output
