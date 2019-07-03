module Main(main) where

import           Foundation

import           Control.Monad        (mapM_)
import qualified Control.Monad.Writer as W
import qualified Data.Text.IO         as T

import           Brainfuck

main :: IO ()
main = mapM_ T.putStrLn . W.execWriter $ interpret "+.+."
