module Back
    ( Exp(..)
    , Com(..)
    , interp
    ) where

import Control.Monad (liftM, ap, when)

-- ## AST Definition

-- Expressões
data Exp = Constant Int
         | Variable String
         | Minus Exp Exp
         | Greater Exp Exp
         | Times Exp Exp
         | Plus Exp Exp
         deriving (Show, Eq)

-- Comandos
data Com = Assign String Exp
         | Seq Com Com
         | Cond Exp Com Com
         | While Exp Com
         | Declare String Exp Com
         | Print Exp
         deriving (Show, Eq)


type Location = Int
type Index = [String]
type Stack = [Int]

position :: String -> Index -> Location
position name index = pos 1 index
  where
    pos n (nm:nms) = if name == nm
                     then n
                     else pos (n + 1) nms
    pos _ []       = error $ "Variable not in scope: " ++ name

fetch :: Location -> Stack -> Int
fetch n (v:vs) = if n == 1
                 then v
                 else fetch (n - 1) vs
fetch _ []     = error "Stack underflow"

put :: Location -> Int -> Stack -> Stack
put n val (v:vs) = if n == 1
                   then val : vs
                   else v : put (n - 1) val vs
put _ _ []       = error "Stack underflow"


newtype M a = StOut { unStOut :: Stack -> (a, Stack, String) }

instance Monad M where
  (>>=) m f = StOut (\stack1 ->
    let (v, stack2, s1) = unStOut m stack1
        (res, stack3, s2) = unStOut (f v) stack2
    in (res, stack3, s1 ++ s2))

instance Functor M where
  fmap = liftM

instance Applicative M where
  pure a = StOut (\stack -> (a, stack, ""))
  (<*>) = ap

getfrom :: Location -> M Int
getfrom loc = StOut (\stack -> (fetch loc stack, stack, ""))

write :: Location -> Int -> M ()
write loc val = StOut (\stack -> ((), put loc val stack, ""))

push :: Int -> M ()
push val = StOut (\stack -> ((), val : stack, ""))

pop :: M ()
pop = StOut (\stack -> case stack of
  [] -> error "Stack empty in pop"
  (v:vs) -> ((), vs, ""))

output :: Show a => a -> M ()
output v = StOut (\stack -> ((), stack, show v))


evall :: Exp -> Index -> M Int
evall exp index = case exp of
  Constant n -> pure n
  Variable x -> let loc = position x index
                in getfrom loc
  Minus e1 e2  -> do { v1 <- evall e1 index; v2 <- evall e2 index; pure (v1 - v2) }
  Greater e1 e2 -> do { v1 <- evall e1 index; v2 <- evall e2 index; pure (if v1 > v2 then 1 else 0) }
  Times e1 e2  -> do { v1 <- evall e1 index; v2 <- evall e2 index; pure (v1 * v2) }
  Plus e1 e2   -> do { v1 <- evall e1 index; v2 <- evall e2 index; pure (v1 + v2) }

exec :: Com -> Index -> M ()
exec stmt index = case stmt of
  Assign name e -> let loc = position name index
                   in do { v <- evall e index;
                           write loc v }
  Seq s1 s2 -> do { exec s1 index;
                    exec s2 index;
                    pure () }
  Cond e s1 s2 -> do { v <- evall e index;
                       when (v == 1) (exec s1 index);
                       when (v == 0) (exec s2 index) }
  While e s -> do { v <- evall e index;
                    when (v == 1) (do { exec s index; exec (While e s) index }) }
  Declare name e stmt -> do { v <- evall e index;
                              push v;
                              exec stmt (name : index);
                              pop }
  Print e -> do { v <- evall e index;
                  output v }

interp :: Com -> ((), Stack, String)
interp a = unStOut (exec a []) []

