{-#LANGUAGE ScopedTypeVariables, LambdaCase, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies, BlockArguments #-}
import Control.Monad.Except
import Control.Applicative
import Data.Char
import Data.Functor
import Data.List

data Expression
  = ECall [Expression]
  | EString String
  | EInteger Integer
  | EWord String
  deriving (Show)

type ParseError = String
newtype Parser a = Parser (String -> (Either [ParseError] a, String))

instance Functor Parser where
  fmap f (Parser something) = Parser (\s -> let (eit, str) = something s in (f <$> eit, str))

instance Applicative Parser where
  pure f = Parser (\s -> (Right f, s))
  (Parser a) <*> (Parser b) = Parser (\s -> let (eit, str) = a s in (let (fs, sn) = b str in (eit <*> fs, sn)))

instance Alternative Parser where
  empty = Parser (\s -> (Left [], s))
  (Parser a) <|> (Parser b) = Parser (\s ->
    let (a', s0) = a s
        (b', s1) = b s0
      in case a' of
        (Left _) -> (b', s1)
        (Right a) -> (a', s0))

instance Monad Parser where
  return a = Parser (\s -> (Right a, s))
  (Parser a) >>= f =
    Parser (\s ->
      let (a', s') = a s
        in case a' of
          Left e -> (Left e, s')
          Right x -> let Parser b = f x in b s')

--x throw :: ParseError -> Parser a
--x try :: Parser a -> Parser a
--x satisfy :: (Char -> Bool) -> Parser Char
--x char :: Char -> Parser ()
--x string :: String -> Parser ()
--x eof :: Parser ()
--x between :: Parser left -> Parser right -> Parser a -> Parser a
--x parens :: Parser a -> Parser a
--x sepBy :: Parser a -> Parser sep -> Parser [a]

eof :: Parser ()
eof = Parser \case
  [] -> (Right (), [])
  s -> (Left ["Not EOF"], s)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy a sep = do
  x <- a
  xs <- manyTill (sep >> a) (sepBy a sep)
  return (x:xs)

between :: Parser left -> Parser right -> Parser a -> Parser a
between left right a = do
  _ <- left
  x <- a
  _ <- right
  return x

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

try :: Parser a -> Parser a
try (Parser p) = Parser \s -> case p s of
  (Left err, _) -> (Left err, s)
  (Right x, s') -> (Right x, s')

throw :: ParseError -> Parser a
throw err = Parser \s -> (Left [err], s)

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill a end =
  end $> []
  <|>
  ((:) <$> a <*> manyTill a end)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser \s -> case s of
  [] -> (Left ["Unexpected EOF"], s)
  (x:xs) -> if f x then (Right x, xs) else (Left ["Unexpected character"], s)

char :: Char -> Parser Char
char c = satisfy (==c)

anyChar :: Parser Char
anyChar = satisfy (const True)

stringLiteral :: Parser String
stringLiteral = do
  char '"'
  manyTill anyChar (char '"')

string :: String -> Parser ()
string str = Parser \s -> case stripPrefix str s of
  Just x -> (Right (), x)
  Nothing -> (Left ["Expected " ++ show str], s)

main :: IO()
main = print "test"