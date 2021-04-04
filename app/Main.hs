{-#LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies #-}
import Control.Monad.Except
import Control.Applicative
import Data.Char

data Types = LParen | RParen | Word String | Text String | Number Double
  deriving (Show, Eq)

isChar :: String -> Bool
isChar str = all (\x -> x `elem` (['a'..'z'] ++ ['A'..'Z'])) str

string :: [Char] -> ([Char], [Char])
string xs = span (/= '"') xs

word :: [Char] -> ([Char], [Char])
word xs = span (\x -> x /= ' ' && isChar [x]) xs

number :: [Char] -> ([Char], [Char])
number xs = span (\x -> (isDigit x || x == '.') && x /= ' ') xs

lexer :: String -> [Types]
lexer [] = []
lexer (x:xs)
  | x == '(' = LParen : lexer xs
  | x == ')' = RParen : lexer xs
  | x == '"' = Text str1 : lexer (drop 1 str2)
  | isDigit x = Number (read num1 :: Double) : lexer num2
  | isChar [x] = Word w1 : lexer w2
  | x == ' ' = lexer xs
  | otherwise = Word [x] : lexer xs

  where _word = [x] ++ xs
        (w1,w2) = word _word
        (str1,str2) = string xs
        (num1,num2) = number _word

newtype Mutable s a = Mutable { runMutable :: s -> (a, s) }

instance Functor (Mutable a) where
  fmap f (Mutable m) = Mutable (\s -> let (s1, s2) = m s in (f s1, s2))

instance Applicative (Mutable a) where
  (Mutable a) <*> (Mutable b) = Mutable (\s ->
    let (fn, s1) = a s
        (s2, s3) = b s1
      in (fn s2, s3))

  pure a = Mutable (\s -> (a, s))

instance Monad (Mutable a) where
  (Mutable a) >>= f =
    Mutable (\s ->
      let (s0, s1) = a s
          Mutable s2 = f s0
        in s2 s1)

get :: Mutable s s
get = Mutable (\s -> (s, s))

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ x = x

eq :: (Eq a) => a -> a -> Bool
eq x y = if' (x == y) True False

exec :: Monad m => [m ()] -> m ()
exec [] = return ()
exec (x:xs) = x >> exec xs

for :: Monad m => [a] -> (a -> m ()) -> m ()
for [] action = return ()
for (x:xs) action = action x >> for xs action

len :: [a] -> Integer
len [] = 0
len (x:xs) = 1 + len xs

index :: (Eq a) => [a] -> a -> Integer
index (x:xs) item | x == item = 0
                  | otherwise = 1 + index xs item

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

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill a end = do
  do { _ <- end; return [] }
  <|>
  do { x <- a; xs <- manyTill (return x) end; return (x:xs) }

main :: IO ()
main = do
  print "test"
  for [1..3] (\x -> print "tes")
  print $ len [1..10]
  print $ index [1..10] 2