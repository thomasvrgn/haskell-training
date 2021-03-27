{-#LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
import Control.Monad.Except
import Control.Applicative
import Control.Monad.State.Lazy
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

main :: IO ()
main = do
  let tokens = lexer "(print test (bruh))"
  print $ tokens

