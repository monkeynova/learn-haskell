module TAP (TAP.pass,TAP.fail,is,done_testing,subtest) where

import Control.Monad.RWS

data TestState = TestState { curTestNum :: Int }

subtest f = runRWS f () (TestState 0)

curTest :: RWS () [String] TestState Int
curTest = do
          state <- get
          return (curTestNum state)

nextTest :: RWS () [String] TestState ()
nextTest = do
           state <- get
           put state{ curTestNum = (curTestNum state) + 1 }
           return ()

showMsg msg = case msg of Nothing -> ""; Just str -> " " ++ str

pass :: Maybe String -> RWS () [String] TestState Bool
pass msg = do 
           nextTest
           testNum <- curTest
           tell [ "ok " ++ (show testNum) ++ showMsg msg ]
           return True

fail :: Maybe String -> RWS () [String] TestState Bool
fail msg = do 
           nextTest
           testNum <- curTest
           tell [ "not ok " ++ (show testNum) ++ showMsg msg ]
           return False
       
is :: Eq a => a -> a -> Maybe String -> RWS () [String] TestState Bool
is got expect msg = do
                    if got == expect
                    then
                        TAP.pass msg
                    else    
                        TAP.fail msg

isnt :: Eq a => a -> a -> Maybe String -> RWS () [String] TestState Bool
isnt got expect msg = do
                      if not( got == expect )
                      then
                          TAP.pass msg
                      else    
                          TAP.fail msg

done_testing :: RWS () [String] TestState Bool
done_testing = do
               testNum <- curTest
               tell [ "1.." ++ (show testNum) ]
               return True

