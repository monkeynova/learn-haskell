{-| cd-fit

An example from The Hitchhiker's Guide to Haskell
http://www.haskell.org/haskellwiki/Hitchhikers_guide_to_Haskell

-}

module Main where

import Control.Monad (liftM2, replicateM)
import Data.List (sortBy,sort)
import Test.QuickCheck
import Text.ParserCombinators.Parsec

data Dir = Dir { dir_size::Int, dir_name::String } deriving Show
data DirPack = DirPack { pack_size::Int, dirs::[Dir] } deriving Show

instance Arbitrary Dir where
--  coarbitrary = undefined
  arbitrary = liftM2 Dir gen_size gen_name
    where
      gen_size = do s <- choose (10,1400); return $ s * 1024 * 1024
      gen_name = do n <- choose (1,300); replicateM n (elements "fubar/")

prop_greedy_pack_is_fixpoint ds = let pack = greedy_pack ds in pack_size pack == pack_size ( greedy_pack (dirs pack) )

-- | dirs = $buf =~ /($dirAndSize)*/
parseInput =
  do
    dirs <- many dirAndSize
    eof :: Parser ()
    return dirs

-- | ( $size, $dir ) = $buf =~ /(\d+)\s+(.*)\n/
dirAndSize =
  do
    size <- many1 digit
    spaces
    dir_name <- anyChar `manyTill` newline
    return (Dir (read size) dir_name)

media_size = 700 * 1024 * 1024

instance Eq Dir where
    (==) a b = dir_size a == dir_size b && dir_name a == dir_name b

instance Ord Dir where
    compare a b = compare (dir_size a) (dir_size b)

greedy_pack dirs =
    foldl maybe_add_dir (DirPack 0 []) $ sort dirs
    where
    cmpSize d1 d2 = compare (dir_size d1) (dir_size d2)

greedy_pack_cmp dirs =
    foldl maybe_add_dir (DirPack 0 []) $ sortBy cmpSize dirs
    where
    cmpSize d1 d2 = compare (dir_size d1) (dir_size d2)

maybe_add_dir p d =
  let
    new_size = pack_size p + dir_size d
    new_dirs = d:(dirs p)
  in
    if new_size > media_size
      then p 
      else DirPack new_size new_dirs
    
main =
  do
    input <- getContents
    let dirs = case parse parseInput "stdin" input of
                    Left err -> error $ "Input:\n" ++ show input ++
                                        "\nError:\n" ++ show err
                    Right result -> result
    putStrLn "Parsed!"
    let packed_dirs = greedy_pack_cmp dirs
    putStrLn "Packed!"
    print packed_dirs
    
