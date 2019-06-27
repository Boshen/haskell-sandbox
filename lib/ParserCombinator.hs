module ParserCombinator where

import           Data.Text  (Text)
import qualified Data.Text  as T
import           Foundation

{--
  Tutorials:
  * http://eprints.nottingham.ac.uk/223/1/pearl.pdf
  * https://eli.thegreenplace.net/2017/deciphering-haskells-applicative-and-monadic-parsers/
--}

{--
  a parser is a function that takes a string of characters as its first argument, and returns a list of results.
  Empty list of results denotes failure of a parser.
  The first component is a value of type `a` produced by parsing and processing a prefix of the argument string.
  The second component is the unparsed suffix of the argument string.
--}
newtype Parser a = Parser (Text -> [(a, Text)])

instance Functor Parser where
  fmap f p = Parser (\cs -> case parse p cs of
                              []         -> []
                              [(a, out)] -> [(f a, out)]
                              _          -> error "Functor")

instance Applicative Parser where
  pure a = Parser (\cs -> [(a, cs)])
  p1 <*> p2 = Parser (\cs -> case parse p1 cs of
                              []         -> []
                              [(a, out)] -> parse (fmap a p2) out
                              _          -> error "Applicative")

instance Monad Parser where
  return a = Parser (\cs -> [(a, cs)])
  p >>= f = Parser (\cs -> mconcat [parse (f a) cs' | (a, cs') <- parse p cs])

class Monad m => MonadZero m where
  zero :: m a

class MonadZero m => MonadPlus m where
  (++) :: m a -> m a -> m a

instance MonadZero Parser where
  zero = Parser (const [])

instance MonadPlus Parser where
  p ++ q = Parser (\cs -> parse p cs <> parse q cs)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p ++ q) cs of
                            []    -> []
                            (x:_) -> [x])

parse :: Parser a -> Text -> [(a, Text)]
parse (Parser p) = p

item :: Parser Char
item = Parser (\cs -> if T.null cs then [] else [(T.head cs, T.tail cs)])

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c -> if p c then return c else zero

char :: Char -> Parser Char
char = satisfy . (==)

string :: Text -> Parser Text
string s = if T.null s
  then return T.empty
  else char (T.head s) >> string (T.tail s) >> return s

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- many p
  return (a:as)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep >> return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
  a <- p
  as <- many (sep >> p)
  return (a:as)
