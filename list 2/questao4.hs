import Test.QuickCheck
import Test.QuickCheck.Gen (Gen(..))

data Expr = Const Integer | Add Expr Expr | Mul Expr Expr | Neg Expr
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- 1. Gen QuickCheck for Expr (genExpr)
-------------------------------------------------------------------------------

-- genExpr :: Int -> Gen Expr
genExpr :: Int -> Gen Expr
genExpr n
  | n <= 0 = Const <$> arbitrary -- Caso base: constante
  | otherwise = frequency
      [ (1, Const <$> arbitrary) -- Probabilidade de gerar uma constante
      , (n, Add <$> genExpr (n `div` 2) <*> genExpr (n `div` 2)) -- Adição: sub-expressões com metade do tamanho
      , (n, Mul <$> genExpr (n `div` 2) <*> genExpr (n `div` 2)) -- Multiplicação: sub-expressões com metade do tamanho
      , (n, Neg <$> genExpr (n - 1)) -- Negação: sub-expressão com tamanho reduzido em 1
      ]

-------------------------------------------------------------------------------
-- 2. Arbitrary for Expr
-------------------------------------------------------------------------------

instance Arbitrary Expr where
  arbitrary = sized genExpr

-- eval :: Expr -> Integer
eval :: Expr -> Integer
eval (Const n)   = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Neg e)     = negate (eval e)

-- Propriedade Distributiva da Multiplicação sobre a Adição
-- A * (B + C) = (A * B) + (A * C)
prop_eval_distributive :: Expr -> Expr -> Expr -> Property
prop_eval_distributive a b c =
  eval (Mul a (Add b c)) === eval (Add (Mul a b) (Mul a c))

-- Propriedade Comutativa da Adição
-- A + B = B + A
prop_eval_add_commutative :: Expr -> Expr -> Property
prop_eval_add_commutative a b =
  eval (Add a b) === eval (Add b a)

-- Propriedade Associativa da Adição
-- A + (B + C) = (A + B) + C
prop_eval_add_associative :: Expr -> Expr -> Expr -> Property
prop_eval_add_associative a b c =
  eval (Add a (Add b c)) === eval (Add (Add a b) c)

-- Propriedade de Dupla Negação
-- -(-A) = A
prop_eval_double_negation :: Expr -> Property
prop_eval_double_negation a =
  eval (Neg (Neg a)) === eval a

-- Propriedade de Identidade da Multiplicação
-- A * 1 = A
prop_eval_mul_identity :: Expr -> Property
prop_eval_mul_identity a =
  eval (Mul a (Const 1)) === eval a


main :: IO ()
main = do
  putStrLn "\n1. Testing: (A * (B + C) = (A * B) + (A * C))"
  quickCheck prop_eval_distributive

  putStrLn "\n2. Testing: (A + B = B + A)"
  quickCheck prop_eval_add_commutative

  putStrLn "\n3. Testing: (A + (B + C) = (A + B) + C)"
  quickCheck prop_eval_add_associative

  putStrLn "\n4. Testing: (-(-A) = A)"
  quickCheck prop_eval_double_negation

  putStrLn "\n5. Testing: (A * 1 = A)"
  quickCheck prop_eval_mul_identity

  putStrLn "\n--- Finishing all tests ---"