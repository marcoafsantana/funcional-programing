import Text.Parsec
import Back (interp)
import Front (languageParser)

main :: IO ()
main = do
  -- Exemplo de programa da sua especificação
  let programText = "declare x = 150 in declare y = 200 in {while x > 0 do {x := x - 1; y := y - 1}; print y}"
  putStrLn $ "Executando o programa:\n" ++ programText ++ "\n"

  -- Tenta analisar e interpretar o programa
  case parse languageParser "program" programText of
    Left err -> putStrLn $ "Erro de parser: " ++ show err
    Right ast -> do
      putStrLn "AST gerada com sucesso:"
      print ast
      putStrLn "\nResultado da execução:"
      let (_, _, outputString) = interp ast
      putStrLn $ outputString

  -- Exemplo de uso para um simples programa interativo.
  putStrLn "\n--- Modo interativo ---"
  putStrLn "Digite um comando ou expressão (ex: print 100):"
  line <- getLine
  case parse languageParser "stdin" line of
    Left err -> putStrLn $ "Erro de parser: " ++ show err
    Right ast -> do
      let (_, _, outputString) = interp ast
      putStrLn $ "Output: " ++ outputString
