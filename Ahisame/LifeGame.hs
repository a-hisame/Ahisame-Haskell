-- ライフゲームの実装
module Ahisame.LifeGame (
 width, height
 , sampleInit, sampleSeq
 , infSeq
 , Pos, Board
 , showBoard, toString
 , next, after
 , animationInf, animation
) where

import System.IO
import Data.List
import Text.Printf
import Control.Applicative

import qualified Ahisame.Console as Console

-- 動作サンプル
-- 呼び出すと、アニメーションでライフゲームが表示されます
exec :: IO ()
exec = animationInf 100 sampleInit 

width :: Int
width = 10

height :: Int
height = 8

-- ライフゲームアニメーションを実行
-- 止まらないので注意
animationInf :: Console.Millis -> Board -> IO ()
animationInf w init = Console.animation w $ toString <$> infSeq init 

-- 有限回数nで停止するライフゲームアニメーションを実行
animation :: Console.Millis -> Board -> Int -> IO ()
animation w init n = Console.animation w $ take n $ toString <$> infSeq init

sampleInit :: Board
sampleInit = [(4,2), (2,3), (4,3), (3,4), (4,4)]

sampleSeq :: [Board]
sampleSeq = infSeq sampleInit

infSeq :: Board -> [Board]
infSeq init = iterate next init 

type Pos = (Int, Int)
type Board = [Pos]

showBoard :: Board -> IO ()
showBoard b = mapM_ putStrLn $ toString b

toString :: Board -> [String]
toString b = bar : ((board b)++ [bar])
 where
  bar :: String
  bar = replicate (2+width) '-'
  board :: Board -> [String] 
  board b = printf "|%s|" . ln b <$> [1..height]
  ln :: Board -> Int -> String
  ln b y = (\x -> if isAlive b (x, y) then 'O' else ' ') <$> [1..width] 

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

neighborhoods :: Pos -> [Pos]
neighborhoods (x, y) = wrap <$> 
 [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
 where
  wrap :: Pos -> Pos
  wrap (x, y) = (((x-1) `mod` width)+1, ((y-1) `mod` height)+1)

liveNeighbors :: Board -> Pos -> Int
liveNeighbors b = length . filter (isAlive b) . neighborhoods

survivors :: Board -> Board 
survivors b = filter (\p -> elem (liveNeighbors b p) [2,3]) b

births :: Board -> [Pos]
births b = filter isBirth $ (,) <$> [1..width] <*> [1..height]
 where
  isBirth :: Pos -> Bool
  isBirth p = (not $ elem p b) && (liveNeighbors b p == 3)

next :: Board -> Board
next b = (survivors b) ++ (births b)

after :: Int -> Board -> Board
after n b = head . drop n $ iterate next b 

