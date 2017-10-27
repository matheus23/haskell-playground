module Lambda.Interpreter where

import Lambda.Calculus
import Lambda.PrettyPrint

import qualified Data.Map as Map
import Data.Map ((!), Map)

data Primitive
  = Number Integer
  | Increment
  deriving (Show)

type Env = Map Name Value

data Value
  = Primitive Primitive
  | Fun Env Name Lambda
  deriving (Show)

executeWith :: Env -> Lambda -> Value
executeWith environment (Var name) = environment ! name
executeWith environment (Abs arg body) = Fun environment arg body
executeWith environment (App func arg) =
  let argValue = executeWith environment arg
   in case executeWith environment func of
        Fun innerEnvironment argName body ->
          executeWith (Map.insert argName argValue innerEnvironment) body
        Primitive Increment ->
          case argValue of
            Primitive (Number n) -> Primitive (Number (n + 1))
            other -> error ("Can only increment, numbers, not this: " ++ show other)
        other -> error ("Cannot apply to something that's not a function: " ++ show other)

executeStdLib :: Lambda -> Value
executeStdLib = executeWith (Map.fromList [("0", Primitive (Number 0)), ("+1", Primitive Increment)])
