import Test.QuickCheck
import Data.List (sort, nub)

take1' :: Int -> [a] -> [a]
take1' _ []      = []
take1' 0 _      = []
take1' n (x:xs) = x : take1' (n-1) xs

take2 :: Int -> [a] -> [a]
take2 0 []       = []
take2 0 (x:xs)   = []
take2 n []       = []
take2 n (x:xs)   = x : take2 (n-1) xs

-- Definição da Árvore de Busca Binária
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- Funções básicas para BST (implementação simplificada para o exemplo)
emptyTree :: Tree a
emptyTree = Empty

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node val left right)
  | x < val   = Node val (insert x left) right
  | x > val   = Node val left (insert x right)
  | otherwise = Node val left right -- Elemento já existe, não insere duplicata

member :: Ord a => a -> Tree a -> Bool
member _ Empty = False
member x (Node val left right)
  | x < val   = member x left
  | x > val   = member x right
  | otherwise = True

toList :: Tree a -> [a]
toList Empty = []
toList (Node val left right) = toList left ++ [val] ++ toList right

-- Instância Arbitrary para Tree a (necessária para QuickCheck gerar árvores)
instance (Arbitrary a, Ord a) => Arbitrary (Tree a) where
  arbitrary = do
    xs <- listOf arbitrary -- Gera uma lista de elementos aleatórios
    return (foldr insert emptyTree xs) -- Insere-os para formar uma BST válida

-- Propriedade 1: Inserir um elemento e depois verificá-lo deve retornar True.
prop_bst_insert_member :: Int -> Tree Int -> Bool
prop_bst_insert_member x tree = member x (insert x tree) == True

-- Propriedade 2: A conversão de uma árvore de busca para uma lista deve resultar em uma lista ordenada sem duplicatas.
prop_bst_toList_sorted_unique :: Tree Int -> Bool
prop_bst_toList_sorted_unique tree = isSorted (toList tree) && noDuplicates (toList tree)
  where
    isSorted [] = True
    isSorted [_] = True
    isSorted (a:b:xs) = a <= b && isSorted (b:xs)

    noDuplicates xs = nub (sort xs) == sort xs -- Mais robusto usando Data.List

-- Propriedade 3: Inserir elementos de uma lista na árvore e converter de volta para uma lista deve ser equivalente a ordenar e remover duplicatas da lista original.
prop_bst_insertList_toList :: [Int] -> Bool
prop_bst_insertList_toList xs = toList (foldr insert emptyTree xs) == (nub . sort) xs

-------------------------------------------------------------------------------
-- Função Principal (main)
-------------------------------------------------------------------------------

main :: IO ()
main = do
  -- "Questão: Dê um exemplo de chamada para take2 que resulta em indefinido, que retornaria um valor
  -- real usando take1. O que isso diz sobre a rigidez (strictness) de cada posição de argumento?"
  -- Para "0 undefined":
  -- take1 -> []
  -- take2 -> undefined
  -- Isso acontece por que no take1, estamos utilizando o wildcard: "_"
  -- take1' 0 _      = []. o 0 dá match e o wildcard tambem. logo retorna []
  -- Enquanto no take2, não existe o wildcard, esperamos uma lista vazia ou lista não vazia.
  -- Logo, o retorno é undefined/runtime error.

  putStrLn "\n--- Testes de Propriedades de Árvore de Busca Binária (BST) ---"
  putStrLn "Testando: Inserir e verificar membro (prop_bst_insert_member)"
  quickCheck prop_bst_insert_member
  putStrLn "Testando: toList resulta em lista ordenada e sem duplicatas (prop_bst_toList_sorted_unique)"
  quickCheck prop_bst_toList_sorted_unique
  putStrLn "Testando: Inserção de lista e toList é consistente com sort/nub (prop_bst_insertList_toList)"
  quickCheck prop_bst_insertList_toList

  putStrLn "\n--- Conclusão de Todos os Testes ---"