import Data.List (nub)

data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop
  deriving (Show, Eq)

type Subst = [(Char, Bool)]

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = case lookup x s of
  Just b -> b
  Nothing -> error "Var not found in substitution"
eval s (Not p) = not (eval s p)
eval s (And p1 p2) = eval s p1 && eval s p2
eval s (Imply p1 p2) = not (eval s p1) || eval s p2

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p1 p2) = nub (vars p1 ++ vars p2)
vars (Imply p1 p2) = nub (vars p1 ++ vars p2)

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) rest ++ map (True :) rest
  where
    rest = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where
    vs = vars p

isTaut :: Prop -> Bool
isTaut p = all (\s -> eval s p) (substs p)

-- example
p1 :: Prop
p1 = Imply (And (Var 'A') (Var 'B')) (Var 'A') -- (A ∧ B) ⇒ A

p2 :: Prop
p2 = Imply (Var 'A') (And (Var 'A') (Var 'B')) -- A ⇒ (A ∧ B)

p3 :: Prop
p3 = Imply (Imply (Imply (Var 'A') (Var 'B')) (Var 'A')) (Var 'A') -- pierce

main :: IO ()
main = do
  putStrLn "##### tautology checker #####"

  putStrLn "\np1: (A ∧ B) ⇒ A"
  putStrLn $ "is tautology? " ++ show (isTaut p1) -- should be: True
  putStrLn "\np2: A ⇒ (A ∧ B)"
  putStrLn $ "is tautology? " ++ show (isTaut p2) -- should be: False
  putStrLn "\n p3 (peirce): ((A ⇒ B) ⇒ A) ⇒ A"
  putStrLn $ "is tautology? " ++ show (isTaut p3) -- should be: True