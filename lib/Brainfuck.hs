module Brainfuck where

import           Foundation

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List            (repeat, (++))
import           Data.Maybe
import           Data.Text            (Text)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

{--
  https://en.wikipedia.org/wiki/Brainfuck
  https://github.com/quchen/articles/blob/master/write_yourself_a_brainfuck.md
  https://github.com/paraseba/haskell-brainfuck/blob/master/src/HaskBF/Parser.hs
--}
data Command
  = MoveRight -- >
  | MoveLeft -- <
  | Increment -- +
  | Decrement -- -
  | Print -- .
  | Read -- ,
  | Loop [Command] -- [ ]
  | Comment Char
  deriving (Eq, Show)

type Source = [Command]

type Parser = Parsec Void Text

{--
  Parser
--}
parser :: Parser Source
parser = many op

op :: Parser Command
op = command <|> loop <|> comment

comment :: Parser Command
comment = Comment <$> noneOf ['>', '<', '+', '-', '.', ',', '[', ']', '?']

loop :: Parser Command
loop = Loop <$> between (char '[') (char ']') parser

command :: Parser Command
command =
  choice
    [ char '>' >> return MoveRight
    , char '<' >> return MoveLeft
    , char '+' >> return Increment
    , char '-' >> return Decrement
    , char '.' >> return Print
    , char ',' >> return Read
    ]

{--
   Interpreter
--}
data Tape a =
  Tape [a] a [a]
  deriving (Show)

emptyTape :: Tape Int
emptyTape = Tape zeroes 0 zeroes
  where
    zeroes = repeat 0

moveRight :: Tape a -> Tape a
moveRight (Tape ls n (r:rs)) = Tape (n : ls) r rs
moveRight tp                 = tp

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) n rs) = Tape ls l (n : rs)
moveLeft tp                 = tp

rewind :: Tape a -> Tape a
rewind (Tape l n r) =
  let (x:xs) = reverse (n : l)
   in Tape [] x (xs ++ r)

getPivot :: Tape a -> a
getPivot (Tape _ n _) = n

newtype StateState =
  StateState
    { tape :: Tape Int
    }

type WriterState = [Int]

type BFState = StateT StateState (Writer WriterState) ()

interpret :: Tape Int -> Text -> [Int]
interpret tp input =
  case parse parser "" input of
    Left bundle -> error . fromList $ errorBundlePretty bundle
    Right source ->
      execWriter $ runStateT (run (sourceToTape source) False) readerState
      where readerState = StateState tp

sourceToTape :: Source -> Tape Command
sourceToTape source = Tape [] (head list) (tail list)
  where
    list = fromJust (nonEmpty source)

updateTape :: Tape Int -> BFState
updateTape tp = modify' (\s -> s {tape = tp})

run :: Tape Command -> Bool -> BFState
run source@(Tape _ middle right) isLoop = do
  tp@(Tape l n r) <- gets tape
  case middle of
    MoveRight -> updateTape (moveRight tp)
    MoveLeft  -> updateTape (moveLeft tp)
    Increment -> updateTape (Tape l (n + 1) r)
    Decrement -> updateTape (Tape l (n - 1) r)
    Print     -> lift $ tell [n]
    Loop lp   -> run (sourceToTape lp) True
    _         -> return ()
  pivot <- getPivot <$> gets tape
  if null right
    then when (isLoop && pivot /= 0) $ run (rewind source) True
    else run (moveRight source) isLoop
