-- Abstract syntax tree for arithmetic expressions of literals
data Exp a = Lit a                 -- constant
           | Add (Exp a) (Exp a)   -- e1 + e2
           | Sub (Exp a) (Exp a)   -- e1 - e2
           | Mul (Exp a) (Exp a)   -- e1 * e2
  deriving (Show)

-- Evaluate (interpret) an arithmetic expression
eval :: Num a => Exp a -> a
eval (Lit n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- Sample expressions
ex1 :: Exp Integer
ex1 = Add (Mul (Lit 5) (Lit 8)) (Lit 2)  --  (5 * 8) + 2

ex2 :: Exp Float
ex2 = Sub (Lit 42.1) (Lit 0.1)           -- 42.1 - 0.1

-- The instruction set of a super-simple stack machine
data Instr a = Push a  -- place literal a on stack top
             | DoAdd   -- add the two top-most stack entries, result at stack top
             | DoSub
             | DoMul
  deriving (Show)

-- A program is a sequence of instructions
type Program a = [Instr a]
-- A stack is a sequence of values (stack top = head)
type Stack a = [a]


-- Compile an expression into a program
compile :: Exp a -> Program a
compile (Lit n)     = [Push n]
compile (Add e1 e2) = compile e1 ++ compile e2 ++ [DoAdd]
compile (Sub e1 e2) = compile e1 ++ compile e2 ++ [DoSub]
compile (Mul e1 e2) = compile e1 ++ compile e2 ++ [DoMul]


-- Run a program against an initial stack
-- (leaves the program result on the stack top)
run :: Num a => Stack a -> Program a -> Stack a
run st []             = st
run st       ((Push n):ins) = run (n:st)       ins
run (x:y:st) (DoAdd:ins)    = run ((y + x):st) ins
run (x:y:st) (DoSub:ins)    = run ((y - x):st) ins
run (x:y:st) (DoMul:ins)    = run ((y * x):st) ins
run _        _              = error "VM program broken"

main :: IO ()
main = do
  print $ ex1
  print $ ex2
  print $ eval ex1
  print $ eval ex2
  print $ compile ex1
  print $ compile ex2
  print $ (run [] . compile) ex1
  print $ (run [] . compile) ex2







