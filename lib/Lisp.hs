module Lisp where

import           Data.Foldable        (foldl1, product, sum)
import           Data.List            (concat, map, unwords)
import           Foundation
import qualified Prelude
import           Text.Parsec
import           Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token    as Token

data AST = I32 Int
         | Sym Prelude.String
         | Nul
         | Err
         | Lst [AST]
         | Boo Bool
         | Nod AST [AST]
         deriving (Eq, Show)

type P = Parsec Prelude.String ()
type Parser = P AST

identifier, operator :: P Prelude.String
symbol :: Prelude.String -> P Prelude.String
parens, lexeme :: P a -> P a
whiteSpace :: P ()
decimal :: P Integer
Token.TokenParser {
        Token.identifier = identifier,
        Token.operator   = operator,
        Token.symbol     = symbol,
        Token.parens     = parens,
        Token.whiteSpace = whiteSpace,
        Token.decimal    = decimal,
        Token.lexeme     = lexeme
    }
    = Token.makeTokenParser emptyDef

number :: Parser
number = lexeme $ I32 . fromIntegral <$> decimal

ident :: Parser
ident = Sym <$> identifier

op :: Parser
op = Sym <$> operator

true :: Parser
false :: Parser
true = try $ const (Boo True) <$> symbol "true"
false = try $ const (Boo False) <$> symbol "false"

nul :: Parser
nul = try $ const Nul <$> symbol "null"

node :: Parser
node = try $ do
    nodes <- parens $ many expression
    return $ case nodes of
        []    -> Nul
        (h:t) -> Nod h t

expression :: Parser
expression = do
    whiteSpace
    choice [true, false, number, nul, ident, op, node]

parser :: Parser
parser = do
    whiteSpace
    ex <- expression
    whiteSpace
    return ex

getI32 :: AST -> Int
getI32 (I32 i) = i
getI32 _       = error "getI32"

isI32 :: AST -> Bool
isI32 (I32 _) = True
isI32 _       = False

func :: Prelude.String -> [AST] -> AST
func "+" ast = if all isI32 ast
    then I32 . sum . fmap getI32 $ ast
    else Err
func "*" ast = if all isI32 ast
    then I32 . product . fmap getI32 $ ast
    else Err
func "-" ast = if all isI32 ast
    then I32 . foldl1 (-) . fmap getI32 $ ast
    else Err
func "/" ast = if all isI32 ast
    then I32 . foldl1 div . fmap getI32 $ ast
    else Err
func "^" ast = case ast of
    [I32 a, I32 b] -> I32 $ a ^ (fromIntegral b :: Word32)
    _              -> Err
func ">" ast = case ast of
    [I32 a, I32 b] -> Boo $ a > b
    _              -> Err
func "<" ast = case ast of
    [I32 a, I32 b] -> Boo $ a < b
    _              -> Err
func "!" ast = case ast of
    [Boo b] -> Boo $ not b
    _       -> Err
func "list" ast = Lst ast
func "size" ast = case ast of
    [Lst ls] -> I32 $ Prelude.length ls
    _        -> Err
func "reverse" ast = case ast of
    [Lst ls] -> Lst $ reverse ls
    _        -> Err
func ".." ast = case ast of
    [I32 a, I32 b] -> Lst $ I32 <$> [a .. b]
    _              -> Err
func "==" ast = case ast of
    [I32 a, I32 b] -> Boo $ a == b
    _              -> Err
func ">=" ast = case ast of
    [I32 a, I32 b] -> Boo $ a >= b
    _              -> Err
func "<=" ast = case ast of
    [I32 a, I32 b] -> Boo $ a <= b
    _              -> Err
func "!=" ast = case ast of
    [I32 a, I32 b] -> Boo $ a /= b
    _              -> Err
func "if" ast = case ast of
    [Boo c, a, b] -> if c then a else b
    [Boo c, a]    -> if c then a else Nul
    _             -> Err
func _ _ = Err

removeNewlines :: Prelude.String -> Prelude.String
removeNewlines = map (\s -> if s `elem` ['\n', '\t', '\r', ','] then ' ' else s)

eval :: AST -> AST
eval (Nod ast list) = case ast of
    s@(Sym _)   -> call s list
    n@(Nod _ _) -> call (eval n) list
    others      -> others
eval others = others

call :: AST -> [AST] -> AST
call (Sym f) list = func f (eval <$> list)
call _ _          = Err

pretty :: AST -> Prelude.String
pretty (I32 i) = Prelude.show i
pretty (Sym s) = s
pretty Nul = "null"
pretty Err = "error"
pretty (Lst asts) = unwords $ pretty <$> asts
pretty (Boo b) = if b then "true" else "false"
pretty (Nod ast asts) = concat ["(", pretty ast, sep, l, ")"]
    where
        l = unwords $ pretty <$> asts
        sep = if null l then "" else " "

parse' :: Prelude.String -> Maybe AST
parse' str = case runParser parser () "" (removeNewlines str) of
    Left _  -> Nothing
    Right s -> Just s

lispPretty :: Prelude.String -> Maybe Prelude.String
lispPretty s = pretty <$> parse' s

lispEval :: Prelude.String -> Maybe AST
lispEval s = eval <$> parse' s
