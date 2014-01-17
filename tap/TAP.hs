{-|

Test Anything Protocol output which mirrors perl's
  Because I'm an old man and set in my ways. Or something.

TODO: 
- Stack trace in failures
- add subtest+plan to metatap.t

|-}

module TAP (TAP.pass,TAP.fail,TAP.or,ok,cmp_ok,is,isnt,note,diag,plan,PlanType(NoPlan,Tests),done_testing,subtest,runTests) where

import Control.Monad.RWS
import GHC.IO.Handle
import System.IO

data PlanType = NoPlan | Tests Int

type TestRead = ()
type TestWrite = [(Handle,String)]
data TestState = TestState {
     curTestNum :: Int,
     isDone :: Bool,
     indent :: String,
     testPlan :: Maybe PlanType,
     failCount :: Int,
     output :: Handle,
     failure_output :: Handle
     }

type TestReturn = RWS TestRead TestWrite TestState Bool

or :: TestReturn -> TestReturn -> TestReturn
or definitely maybe = do
                      isOK <- definitely
                      if isOK
                      then
                          return isOK
                      else
                          maybe

defaultState :: TestState
defaultState = TestState{
               curTestNum=0,
               isDone=False,
               indent="",
               testPlan=Nothing,
               failCount=0,
               output=stdout,
               failure_output=stderr
             }

runTests :: TestReturn -> IO ()
runTests f = do
             newout <- hDuplicate stdout
             newerr <- hDuplicate stderr
             let (isOK,s,output) = runRWS (do f; runTestsEnd) () defaultState{output=newout, failure_output=newerr}
             mapM_ (\(h,s) -> do hPutStrLn h s; hFlush h) output

runTestsEnd :: TestReturn
runTestsEnd = do
              state <- get
              runTestsEndCheckPlan
              runTestsEndCheckFail

runTestsEndTestPlural :: Int -> String
runTestsEndTestPlural n = if n > 1 then "tests" else "test"

runTestsEndCheckFailAddRun :: TestState -> String
runTestsEndCheckFailAddRun s = case testPlan s of
                                    Just (Tests n) -> if n < curTestNum s then " run" else ""
                                    _ -> ""

runTestsEndCheckFail :: TestReturn
runTestsEndCheckFail = do
                       state <- get
                       if (failCount state) > 0
                       then
                           do
                           diag $ "Looks like you failed " ++
                                  (show $ failCount state) ++ " " ++
                                  (runTestsEndTestPlural $ failCount state)  ++ " of " ++ 
                                  (show $ curTestNum state) ++ 
                                  (runTestsEndCheckFailAddRun state) ++   "."
                           return False
                       else
                           do
                           return True

runTestsEndCheckPlan :: TestReturn
runTestsEndCheckPlan = do
                       state <- get
                       case (testPlan state) of
                            Nothing -> do
                                       diag "Tests were run but no plan was declared and done_testing() was not seen."
                                       return True
                            Just NoPlan -> if (isDone state)
                                           then
                                               do
                                               return True
                                           else
                                               do
                                               done_testing
                            Just (Tests n) -> if n == (curTestNum state)
                                              then
                                                  do
                                                  return True
                                              else
                                                  do
                                                  diag $ "Looks like you planned " ++ 
                                                         (show n) ++ " " ++
                                                         (runTestsEndTestPlural n) ++ " but ran " ++ 
                                                         (show $ curTestNum state) ++ "."
                                                  return False

plan :: PlanType -> TestReturn
plan userPlan = do
                state <- get
                modify (\s -> s{testPlan = Just userPlan})
                case userPlan of
                     Tests count -> do tell [ ((output state), (indent state) ++ "1.." ++ (show count)) ]; return True
                     NoPlan -> do return True
                

subtestState :: TestState -> TestState
subtestState state = state{
                        curTestNum=0,
                        isDone=False,
                        indent=((indent state) ++ "    "),
                        testPlan=Just NoPlan,
                        failCount=0
                     }

subtest :: Maybe String -> TestReturn -> TestReturn
subtest subtest f = do
                    state <- get
                    let (isOK,s,output) = runRWS (do f; runTestsEnd) () (subtestState state)
                    tell output
                    ok isOK subtest

nextTest :: TestState -> TestState
nextTest state = state{curTestNum = (curTestNum state) + 1}

showMsg msg = case msg of Nothing -> ""; Just str -> " - " ++ str

pass :: Maybe String -> TestReturn
pass msg = do 
           modify nextTest
           state <- get
           tell [ ((output state), (indent state) ++ "ok " ++ (show $ curTestNum state) ++ showMsg msg) ]
           return True

fail :: Maybe String -> TestReturn
fail msg = do 
           modify nextTest
           state <- get
           modify (\s -> s{failCount = (failCount state) + 1})
           tell [ ((output state), (indent state) ++ "not ok " ++ (show $ curTestNum state) ++ showMsg msg) ]
           case msg of
                Just str -> do diag $ "  Failed test '" ++ str ++ "'"; return False
                Nothing -> do diag $ "  Failed test"; return False
       
note :: String -> TestReturn
note msg = do
           state <- get
           let prefix = (indent state) ++ "# "
           tell [ ((output state), prefix ++ msg) ]
           return True

diag :: String -> TestReturn
diag msg = do
           state <- get
           let prefix = (indent state) ++ "# "
           tell [ ((failure_output state), prefix ++ msg) ] 
           return True

ok :: Bool -> Maybe String -> TestReturn
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

cmp_ok :: (Ord a,Show a) => a -> Ordering -> a -> Maybe String -> TestReturn
cmp_ok got cmp expect msg = ok (got `compare` expect == cmp) msg `TAP.or` do
                                diag $ "     '" ++ (show got) ++ "'"
                                diag $ "         " ++ (cmp_show cmp)
                                diag $ "     '" ++ (show expect) ++ "'"
                                return False

is :: (Eq a,Show a) => a -> a -> Maybe String -> TestReturn
is got expect msg = ok (got == expect) msg `TAP.or` do
                        diag $ "         got: '" ++ (show got) ++ "'"
                        diag $ "    expected: '" ++ (show expect) ++ "'"
                        return False

isnt :: (Eq a,Show a) => a -> a -> Maybe String -> TestReturn
isnt got expect msg = do
                      if not( got == expect )
                      then
                          TAP.pass msg
                      else    
                        do
                        TAP.fail msg
                        diag $ "         got: '" ++ (show got) ++ "'"
                        diag $ "    expected: anything else"
                        return False

done_testing :: TestReturn
done_testing = do
               state <- get
               if isDone state
               then
                   do
                   TAP.fail $ Just "done_testing() was already called"
               else
                   do
                   modify (\s -> s{isDone = True,testPlan=Just (Tests $ curTestNum state)})
                   tell [ ((output state), (indent state) ++ "1.." ++ (show $ curTestNum state) ) ]
                   if curTestNum state < 1
                   then
                        do
                        diag "No tests run!"
                        return True
                   else
                        do
                        return True

