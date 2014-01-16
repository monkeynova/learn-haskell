module TAP (TAP.pass,TAP.fail,ok,cmp_ok,is,isnt,note,diag,plan,PlanType(NoPlan,Tests),done_testing,subtest,run_tests) where

import Control.Monad.RWS
import System.IO

data PlanType = NoPlan | Tests Int

type TestRead = ()
type TestWrite = [(Handle,String)]
data TestState = TestState { curTestNum :: Int, isDone :: Bool, indent :: String, testPlan :: Maybe PlanType }

type TestReturn = RWS TestRead TestWrite TestState

or :: TestReturn Bool -> TestReturn Bool -> TestReturn Bool
or definitely maybe = do
                      isOK <- definitely
                      if isOK
                      then
                          return isOK
                      else
                          maybe

run_tests :: TestReturn Bool -> IO ()
run_tests f = do
              let (isOK,s,output) = runRWS (do f; done_testing) () (TestState 0 False "" Nothing)
              mapM_ (\(h,s) -> hPutStrLn h s) output

plan :: PlanType -> TestReturn ()
plan userPlan = do
                modify (\s -> s{testPlan = Just userPlan})
                case userPlan of
                     Tests count -> do tell [ (stdout, "1.." ++ (show count)) ]
                     NoPlan -> do return ()
                

subtest :: Maybe String -> TestReturn Bool -> TestReturn Bool
subtest subtest f = return True

nextTest :: TestReturn ()
nextTest = do
           state <- get
           put state{ curTestNum = (curTestNum state) + 1 }
           return ()

showMsg msg = case msg of Nothing -> ""; Just str -> " - " ++ str

pass :: Maybe String -> TestReturn Bool
pass msg = do 
           nextTest
           state <- get
           tell [ (stdout, (indent state) ++ "ok " ++ (show $ curTestNum state) ++ showMsg msg) ]
           return True

fail :: Maybe String -> TestReturn Bool
fail msg = do 
           nextTest
           state <- get
           tell [ (stdout, (indent state) ++ "not ok " ++ (show $ curTestNum state) ++ showMsg msg) ]
           return False
       
note :: String -> TestReturn ()
note msg = do
           state <- get
           tell [ (stdout, (indent state) ++ "# " ++ msg) ]
           return ()

diag :: String -> TestReturn ()
diag msg = do
           state <- get
           tell [ (stderr, (indent state) ++ "# " ++ msg) ]
           return ()

ok :: Bool -> Maybe String -> TestReturn Bool
ok test msg = do
              if test
              then
                  TAP.pass msg
              else    
                  TAP.fail msg

cmp_show :: Ordering -> String
cmp_show cmp = case cmp of
               GT -> ">"
               LT -> "<"
               EQ -> "=="

cmp_ok :: (Ord a,Show a) => a -> Ordering -> a -> Maybe String -> TestReturn Bool
cmp_ok got cmp expect msg = ok (got `compare` expect == cmp) msg `TAP.or` do
                        diag $ "     '" ++ (show got) ++ "'"
                        diag $ "         " ++ (cmp_show cmp)
                        diag $ "     '" ++ (show expect) ++ "'"
                        return False

is :: (Eq a,Show a) => a -> a -> Maybe String -> TestReturn Bool
is got expect msg = ok (got == expect) msg `TAP.or` do
                        diag $ "          got = '" ++ (show got) ++ "'"
                        diag $ "     expected = '" ++ (show expect) ++ "'"
                        return False

isnt :: (Eq a,Show a) => a -> a -> Maybe String -> TestReturn Bool
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

done_testing :: TestReturn Bool
done_testing = do
               state <- get
               if isDone state
               then
                   do
                   TAP.fail $ Just "done_testing was already called"
               else
                   do
                   modify (\s -> s{isDone = True})
                   tell [ (stdout, (indent state) ++ "1.." ++ (show $ curTestNum state) ) ]
                   return True

