-- | Interp

module Interp where

import qualified Data.HashMap.Lazy as M

import Types

eval :: Expr -> Env -> Int -- You will almost certainly need to change this to use your own Value type.
eval (ENum i) _ = i

-- Use this function as your top-level entry point so you don't break `app/Main.hs`

run :: Core -> String
run prog =
  case M.lookup "main" prog of
    Nothing -> error "Supercombinator main not defined."
    Just (_,[],mainBody) ->
      let result = eval mainBody (M.empty)
       in show result
  














