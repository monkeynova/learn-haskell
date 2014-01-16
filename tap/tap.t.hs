module Main where

import TAP

mainTests = do
            plan $ Tests 1
            TAP.pass $ Just "Bar"
            TAP.pass Nothing
            TAP.fail $ Just "test failure"
            is 1 1 $ Just "is true"
            is 1 2 $ Just "is false"
            cmp_ok 1 LT 2 $ Just "cmp_ok true"
            cmp_ok 1 GT 2 $ Just "cmp_ok false"
            subtest (Just "subtest-pass") ( do
                    plan NoPlan
                    TAP.pass $ Just "subtest sub-pass"
                    note "subtest note"
               )
            subtest (Just "subtest-fail") ( do
                    plan $ Tests 3
                    TAP.pass $ Just "subtest sub-pass"
                    TAP.fail $ Just "subtest sub-fail"
                    diag "subtest diag"
               )

main = runTests mainTests 
