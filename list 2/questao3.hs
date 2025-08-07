import Test.QuickCheck
import Set -- Importa o módulo Set que você forneceu

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = do
    xs <- listOf arbitrary
    return (makeSet xs)

-- 1. Identidade da União: A união de um conjunto com o conjunto vazio é o próprio conjunto.
-- A U {} = A
prop_set_union_identity :: (Ord a, Eq a) => Set a -> Bool
prop_set_union_identity s = (s `union` empty) `eqSet` s

-- 2. Comutativa da Interseção: A ordem dos operandos na interseção não altera o resultado.
-- A inter B = B inter A
prop_set_inter_commutative :: (Ord a, Eq a) => Set a -> Set a -> Bool
prop_set_inter_commutative s1 s2 = (s1 `inter` s2) `eqSet` (s2 `inter` s1)

-- 3. Diferença em termos de Interseção: A \ B = A \ (A inter B)
prop_set_diff_inter_relation :: (Ord a, Eq a) => Set a -> Set a -> Bool
prop_set_diff_inter_relation s1 s2 = (s1 `diff` s2) `eqSet` (s1 `diff` (s1 `inter` s2))

-- 4. Associativa da Interseção: A inter (B inter C) = (A inter B) inter C
prop_set_inter_associative :: (Ord a, Eq a) => Set a -> Set a -> Set a -> Bool
prop_set_inter_associative s1 s2 s3 =
  (s1 `inter` (s2 `inter` s3)) `eqSet` ((s1 `inter` s2) `inter` s3)

-- 5. Relação entre Subconjunto e União: Se A é um subconjunto de B, então A U B = B.
prop_set_subset_union_relation :: (Ord a, Show a) => Set a -> Set a -> Property
prop_set_subset_union_relation s1 s2 =
  (s1 `subSet` s2) ==> (s1 `union` s2) `eqSet` s2

-- Inicialmente, utilizei a 5 propiedade com o condicional e sem a geração de pares com subconjuntos. Mas isso me retornava um alto descarte dos testes.
-- Dessa forma, utilizei a genSubsetPair para gerar conjunto + subconjunto e apenas passar eles para a segunda parte da propiedade.
-- Gerador auxiliar de conjunto + subconjunto para que o quickCheck desta propiedade não falhe por descarte(primeira condição, de ser subconjunto, falhar).
genSubsetPair :: (Arbitrary a, Ord a) => Gen (Set a, Set a)
genSubsetPair = do
  superset <- arbitrary
  elementsFromSuperset <- shuffle (flatten superset)
  numSubsetElems <- choose (0, length elementsFromSuperset)
  subsetElems <- take numSubsetElems <$> shuffle elementsFromSuperset
  let subset = makeSet subsetElems
  return (subset, superset) -- Retorna o par (subconjunto, superconjunto)


main :: IO ()
main = do
  putStrLn "--- Testes de Propriedades Matemáticas de Conjuntos (Set) ---"

  putStrLn "\n1. Testing: Identidade da União (A U {} = A)"
  quickCheck (prop_set_union_identity :: Set Int -> Bool)

  putStrLn "\n2. Testing: Comutatividade da Interseção (A inter B = B inter A)"
  quickCheck (prop_set_inter_commutative :: Set Int -> Set Int -> Bool)

  putStrLn "\n3. Testing: Relação Diferença/Interseção (A \\ B = A \\ (A inter B))"
  quickCheck (prop_set_diff_inter_relation :: Set Int -> Set Int -> Bool)

  putStrLn "\n4. Testing: Associatividade da Interseção (A inter (B inter C) = (A inter B) inter C)"
  quickCheck (prop_set_inter_associative :: Set Int -> Set Int -> Set Int -> Bool)

  putStrLn "\n5. Testing: Relação Subconjunto/União (Se A subconjunto B, então A U B = B)"
  quickCheck $ forAll (genSubsetPair :: Gen (Set Int, Set Int)) (uncurry prop_set_subset_union_relation)


  putStrLn "\n--- Conclusão de Todos os Testes ---"