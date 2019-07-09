module Main(main) where

import           Foundation
import qualified Prelude       as P

import           Control.Monad (mapM_)

import           Brainfuck

main :: IO ()
main = mapM_ P.print $ interpret emptyTape "+.+."
