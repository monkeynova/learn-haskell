module TAP (TAP.pass,TAP.fail,is,isnt,note,diag,done_testing,subtest,run_tests) where

import Control.Monad.RWS

data TestState = TestState { curTestNum :: Int, isDone :: Bool }

run_tests :: RWS () [String] TestState Bool -> IO ()
run_tests f = do
              let (isOK,s,output) = subtest f
              mapM_ putStrLn output


subtest :: RWS () [String] TestState Bool -> (Bool, TestState, [String])
subtest f = runRWS (do f; done_testing) () (TestState 0 False)

nextTest :: RWS () [String] TestState ()
nextTest = do
           state <- get
           put state{ curTestNum = (curTestNum state) + 1 }
           return ()

showMsg msg = case msg of Nothing -> ""; Just str -> " - " ++ str

pass :: Maybe String -> RWS () [String] TestState Bool
pass msg = do 
           nextTest
           state <- get
           tell [ "ok " ++ (show $ curTestNum state) ++ showMsg msg ]
           return True

fail :: Maybe String -> RWS () [String] TestState Bool
fail msg = do 
           nextTest
           state <- get
           tell [ "not ok " ++ (show $ curTestNum state) ++ showMsg msg ]
           return False
       
note :: String -> RWS () [String] TestState ()
note msg = do
           tell [ "# " ++ msg ]
           return ()

diag :: String -> RWS () [String] TestState ()
diag msg = do
           tell [ "# " ++ msg ]
           return ()

is :: (Eq a,Show a) => a -> a -> Maybe String -> RWS () [String] TestState Bool
is got expect msg = do
                    if got == expect
                    then
                        TAP.pass msg
                    else    
                        do
                        TAP.fail msg
                        diag $ "         got = '" ++ (show got) ++ "'"
                        diag $ "    expected = '" ++ (show expect) ++ "'"
                        return False

isnt :: (Eq a,Show a) => a -> a -> Maybe String -> RWS () [String] TestState Bool
isnt got expect msg = do
                      if not( got == expect )
                      then
                          TAP.pass msg
                      else    
                        do
                        TAP.fail msg
                        diag $ "         got = '" ++ (show got) ++ "'"
                        diag $ "    expected = anything else"
                        return False

done_testing :: RWS () [String] TestState Bool
done_testing = do
               state <- get
               if isDone state
               then
                   do
                   TAP.fail $ Just "done_testing was already called"
               else
                   do
                   modify (\s -> s{isDone = True})
                   tell [ "1.." ++ (show $ curTestNum state) ]
                   return True

