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

type ParseError = String
newtype Parser a = Parser (String -> (Either [ParseError] a, String))

instance Functor Parser where
  fmap f (Parser something) = Parser (\s -> let (eit, str) = something s in (f <$> eit, str))

instance Applicative Parser where
  pure f = Parser (\s -> (Right f, s))
  (Parser a) <*> (Parser b) = Parser (\s -> let (eit, str) = a s in (let (fs, sn) = b str in (eit <*> fs, sn)))

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

main :: IO ()
main = do
  let (fs, sn) = runMutable (test) 5
  print fs