module TAP (TAP.pass,TAP.fail,is,done_testing) where

import Data.IORef;
import System.IO.Unsafe;

data TestState = TestState { testNum::Int }

{-# NOINLINE funcs #-}
funcs :: IORef (TestState)
funcs = unsafePerformIO $ newIORef (TestState 0)

incTestNum :: IO ()
incTestNum = atomicModifyIORef funcs (\s -> (s{testNum = (testNum s) + 1},()))

showMsg msg = case msg of Nothing -> ""; Just str -> " " ++ str

pass :: Maybe String -> IO ()
pass msg = do
           incTestNum
           curState <- readIORef funcs
           putStrLn $ "ok " ++ (show (testNum curState)) ++ (showMsg msg)

fail :: Maybe String -> IO ()
fail msg = do
           incTestNum
           curState <- readIORef funcs
           putStrLn $ "not ok " ++ (show (testNum curState)) ++ (showMsg msg)

is :: Eq a => a -> a -> Maybe String -> IO ()
is got expect msg = do
                    if got == expect
                    then TAP.pass msg
                    else TAP.fail msg

done_testing = do
               curState <- readIORef funcs
               putStrLn $ "1.." ++ (show (testNum curState))
