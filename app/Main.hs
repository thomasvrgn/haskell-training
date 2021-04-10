{-# LANGUAGE BlockArguments, LambdaCase #-}
module Main where
  import Data.List
  import Mutable
  import Optional

  data Expr
    = Call [Expr]
    | Num Integer
    | Str String
    | Var String
    deriving (Show, Eq)

  type Stack = [String]

  ePrint :: IO (Optional Expr) -> IO (Optional Expr)
  ePrint s = do
    wrap <- s
    let elem = getOk wrap
    case elem of
      (Num a) -> putStr . show $ a
      (Str a) -> putStr a
    putStr " "
    return wrap

  eval :: Expr -> IO (Optional Expr)
  eval (Call (x:xs))
    | x == Var "print" = do
      let elements = map eval xs
      mapM (ePrint) elements >> putStr "\n"
      return Undefined

  eval x@(Num a) = return $ Ok x
  eval x@(Str a) = return $ Ok x

  main :: IO (Optional Expr)
  main = eval $ Call [Var "print", Str "Hello", Num 4]