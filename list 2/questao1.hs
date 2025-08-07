import Test.QuickCheck
import Data.List (intercalate)

-- Propriedade 1: xs ++ [] = xs
prop_appendEmpty :: [Int] -> Bool
prop_appendEmpty xs = (xs ++ []) == xs

-- Propriedade 2: xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
prop_appendAssociative :: [Int] -> [Int] -> [Int] -> Bool
prop_appendAssociative xs ys zs = (xs ++ (ys ++ zs)) == ((xs ++ ys) ++ zs)

-- Função principal para rodar os testes
main :: IO ()
main
  = do
      putStrLn "Testando a propriedade: xs ++ [] = xs"
      quickCheck prop_appendEmpty

      putStrLn "\nTestando a propriedade: xs ++ (ys ++ zs) = (xs ++ ys) ++ zs"
      quickCheck prop_appendAssociative

      putStrLn "\nTodos os testes concluídos."