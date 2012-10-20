{-
 Console上でグラフィカルな効果を表したいときに、
 それをエスケープシーケンスで実現するための関数群。
 Mac (Lion) でのみ動作確認
-}
module Ahisame.Console(
 Millis,
 clear, 
 goto, 
 screenInit,
 waitMillis,
 animation
) where

import System.IO
import Text.Printf
import Control.Concurrent

-- 座標の型
type Pos = (Int, Int)
type Millis = Int

-- 画面の全表示をクリア
clear :: IO ()
clear = putStr "\ESC[2J"

-- カーソルを任意の位置に移動
goto :: Pos -> IO ()
goto (x, y) = putStr $ printf "\ESC[%d;%dH" x y

-- 画面を初期化し、左上にカーソルをあわせる
screenInit :: IO ()
screenInit = do
 clear
 goto (0, 0)

-- 引数のミリ秒分waitする
waitMillis :: Millis -> IO ()
waitMillis millis = threadDelay (millis*1000)

-- 文字列のListを規定間隔でアニメーション表示する
-- 描画位置は(0, 0)を基準とする
animation :: Millis -> [[String]] -> IO ()
animation _ [] = return ()
animation time (x:xs) = do
 goto (0, 0)
 clear
 mapM_ putStrLn x
 waitMillis time
 animation time xs


