module Turtles where

import Control.Monad.Trans.State.Lazy

-------------------------------------------------------------------------------
-- Type defintions; the show instances here should be redone with the HughesPJ
-- library when someone is feeling less lazy.

type Variable = String
type Symbol = String
type Env = [(Variable, Expr)]

type MachineState = (Env, [Expr])
emptyState = ([], [])

type Value = Either Thunk Prim

data Thunk = Thunk Expr Env Env deriving Show
data Prim  = I Int | B Bool | V Variable | S Symbol | Nil deriving Show
data Expr  = P Prim | Op Oper | Eval | BlockStart | BlockEnd
  deriving Show 
data Oper  = Plus | Times deriving Show

-------------------------------------------------------------------------------
-- Monadic operations to make dealing with the underlying machine state easier

getStack :: State MachineState [Expr]
getStack = do (env, stack) <- get
              return stack

getEnv :: State MachineState Env
getEnv = do (env, stack) <- get
            return env

putMachineState :: Env -> [Expr] -> State MachineState ()
putMachineState env stack = put (env,stack)

putStack :: [Expr] -> State MachineState ()
putStack stack = do (env, _) <- get
                    putMachineState env stack

putEnv :: Env -> State MachineState ()
putEnv env = do (_, stack) <- get
                putMachineState env stack

-------------------------------------------------------------------------------
-- Stack operators and the underlying machine.
--   push will push the expression
--   forceStack will force the next stack operation
--   eval will do evaluation

push :: Expr -> State MachineState ()
push x = do {stack <- getStack; putStack (x:stack)}

forceStack :: State MachineState ()
forceStack = 
  do stack <- getStack
     case stack of
      ((Op op):ss) -> 
        case op of
          Plus  -> case ss of
                    ((P (I fst)):((P (I snd)):rest)) -> 
                      putStack ((P $ I $ (fst + snd)):rest)
                    _                -> error "Invalid or incorrect number of arguments to + ."
          Times -> case ss of
                    ((P (I fst)):((P (I snd)):rest)) -> 
                      putStack ((P $ I $ (fst * snd)):rest)
                    _                -> error "Invalid or incorrect number of arguments to * ."
      _            -> return ()

eval :: [Expr] -> State MachineState (Either [Expr] Expr)
eval []        = do res <- getStack
                    return (Left res)
eval (Eval:xs) = do forceStack
                    eval xs
eval (x:xs)    = do push x
                    eval xs

-------------------------------------------------------------------------------
-- The end user interface (run) and some test programs.

run :: [Expr] -> [Expr]
run e = case runState (eval e) emptyState of
          (Left res, _)  -> res
          (Right res, _) -> [res] 

t1 = [P $ I $ 5, P $ I $ 3, Op Plus]
t2 = [P $ I $ 5, P $ I $ 3, Op Plus, Eval]

-------------------------------------------------------------------------------
-- Thoughts:
-- It would be nice to write pop :: State [Expr] Expr, but there are a lot of
-- design considerations that just have blanks:
--   - What should you do when you pop an empty stack? 
--   - What about undersaturation of operators?
--       What is 3 + .
--       Is this 3? If so, then 1 2 3 + . => 6 ?
--   - How do blocks work? 
--       What is 7 { 2 3 + } .
--      
