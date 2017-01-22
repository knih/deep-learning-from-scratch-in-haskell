module Ch01.Basics where

import qualified Data.Map       as Map
import           Control.Monad

-- 算術演算
op :: IO ()
op = do
  putStrLn $ show $ (1 - 2 :: Integer)
  putStrLn $ show $ (4 * 5 :: Integer)
  putStrLn $ show $ (7 / 5 :: Double)
  putStrLn $ show $ (3 ** 2 :: Double)

-- データ型
-- λ> :t 10
-- 10 :: Num t => t
-- λ> :t 2.718
-- 2.718 :: Fractional t => t
-- λ> :t "hello"
-- "hello" :: [Char]

-- 変数
var :: IO ()
var = do
  let x = 10
  putStrLn $ show x
  let x = 100 -- variable shadowing
  putStrLn $ show x
  let y = 3.14
  putStrLn $ show $ (x * y :: Double)

-- リスト
list_ :: IO ()
list_ = do
  let a = [1,2,3,4,5]
  putStrLn $ show a
  putStrLn $ show $ length a
  putStrLn $ show $ a !! 0
  putStrLn $ show $ a !! 4
  let a' = replace 4 99 a
  putStrLn $ show a'

  -- splicing
  putStrLn $ show $ take 2 a' -- [0:2]
  putStrLn $ show $ drop 1 a' -- [1:]
  putStrLn $ show $ take 3 a' -- [:3]
  putStrLn $ show $ take 3 a' -- [:3]
  putStrLn $ show $ take ((length a') -1) a' -- [:-1]
  putStrLn $ show $ take ((length a') -2) a' -- [:-2]

  where replace n v (x:xs)
          | n == 0    = v:xs
          | otherwise = x:replace (n - 1) v xs

-- ディクショナリ
dict :: IO ()
dict = do
  putStrLn $ show $ Map.lookup "height" me

  where
    me  = Map.fromList [ ("height", 180)] :: Map.Map String Integer
    me' = Map.insert "weight" 70 me
-- => Just 180
--    fromList [("height",180),("weight",70)]

-- ブーリアン
bool_ :: IO ()
bool_ = do
  let hungry = True
  let sleepy = False
  putStrLn $ show $ not hungry
  putStrLn $ show $ hungry && sleepy
  putStrLn $ show $ hungry || sleepy

-- if
if_ :: IO ()
if_ = do
  let hungry = True
  when (hungry) $ putStrLn "I'm hungry"
  let hungry' = False
  if hungry'
    then putStrLn "I'm hungry"
    else putStrLn "I'm not hungry" >> putStrLn "I'm sleepy"

-- for
for_ :: IO ()
for_ = do
  forM_ [1,2,3] $ \i -> do
    print i

-- 関数
hello :: IO ()
hello = putStrLn "Hello World!"

hello' :: Show a => a -> IO ()
hello' object = putStrLn $ "Hello " ++ show object ++ "!"
-- λ> hello' "cat"
-- Hello "cat"!
