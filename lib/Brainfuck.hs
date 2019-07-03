module Brainfuck where

import           Foundation

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List            (repeat)
import           Data.Maybe
import           Data.Text            (Text)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

{--
  https://github.com/quchen/articles/blob/master/write_yourself_a_brainfuck.md
  https://github.com/paraseba/haskell-brainfuck/blob/master/src/HaskBF/Parser.hs
--}

data Command = MoveRight      -- >
             | MoveLeft       -- <
             | Increment      -- +
             | Decrement      -- -
             | Print          -- .
             | Read           -- ,
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
comment = Comment <$> noneOf ['>', '<', '+', '-', '.', ',', '[', ']']

loop :: Parser Command
loop = Loop <$> between (char '[') (char ']') parser

command :: Parser Command
command = choice [ char '>' >> return MoveRight
                 , char '<' >> return MoveLeft
                 , char '+' >> return Increment
                 , char '-' >> return Decrement
                 , char '.' >> return Print
                 , char ',' >> return Read
                 ]

{--
   Interpreter
--}

data Tape a = Tape [a] a [a] deriving (Show)

emptyTape :: Tape Integer
emptyTape = Tape zeroes 0 zeroes
  where zeroes = repeat 0

moveRight :: Tape a -> Tape a
moveRight (Tape ls n (r:rs)) = Tape (n:ls) r rs
moveRight tp                 = tp

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) n rs) = Tape ls l (n:rs)
moveLeft tp                 = tp

data StateState = StateState
  { tape   :: Tape Integer
  , isLoop :: Bool
  }
type WriterState = [Integer]

type BFState = StateT StateState (Writer WriterState) ()

interpret :: Tape Integer -> Text -> [Integer]
interpret tp input = case parse parser "" input of
  Left bundle  -> error . fromList $ errorBundlePretty bundle
  Right source ->
    execWriter $ runStateT (run (sourceToTape source)) readerState
    where readerState = StateState tp False

sourceToTape :: Source -> Tape Command
sourceToTape source = Tape [] (head list) (tail list)
  where
    list = fromJust (nonEmpty source)

log :: Integer -> BFState
log s = lift $ tell [s]

updateTape :: Tape Integer -> BFState
updateTape tp = modify' (\s -> s { tape = tp})

updateIsLoop :: Bool -> BFState
updateIsLoop b = modify' (\s -> s { isLoop = b})

run :: Tape Command -> BFState
run source = do
  tp@(Tape l n r) <- gets tape
  case source of
    Tape _ MoveRight _ -> updateTape (moveRight tp)

    Tape _ MoveLeft _  -> updateTape (moveLeft tp)

    Tape _ Increment _ -> updateTape (Tape l (n + 1) r)

    Tape _ Decrement _ -> updateTape (Tape l (n - 1) r)

    Tape _ Print _     -> log n

    Tape _ (Loop lp) _ -> do
      updateIsLoop True
      run (sourceToTape lp)

    _                  -> return ()

  advance source

advance :: Tape Command -> BFState
advance (Tape l n []) = do
  (Tape _ m _) <- gets tape
  isLp <- gets isLoop
  when isLp $ if m == 0
    then updateIsLoop False
    else let (x:xs) = reverse (n:l) in run (Tape [] x xs)
advance source        = run (moveRight source)
