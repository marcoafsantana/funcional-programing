module Front
    ( languageParser
    ) where

import Text.Parsec
import Text.Parsec.String
import Back (Exp(..), Com(..))

type MyParser = Parsec String ()

parens :: MyParser a -> MyParser a
parens p = between (char '(' <* spaces) (spaces *> char ')') p

braces :: MyParser a -> MyParser a
braces p = between (char '{' <* spaces) (spaces *> char '}') p

semicolon :: MyParser Char
semicolon = spaces *> char ';' <* spaces

colonEqual :: MyParser String
colonEqual = string ":="

inKeyword :: MyParser String
inKeyword = string "in"

doKeyword :: MyParser String
doKeyword = string "do"

ifKeyword :: MyParser String
ifKeyword = string "if"

thenKeyword :: MyParser String
thenKeyword = string "then"

elseKeyword :: MyParser String
elseKeyword = string "else"

whileKeyword :: MyParser String
whileKeyword = string "while"

declareKeyword :: MyParser String
declareKeyword = string "declare"

printKeyword :: MyParser String
printKeyword = string "print"

number :: MyParser Exp
number = Constant . read <$> many1 digit

variable :: MyParser Exp
variable = Variable <$> many1 letter

factor :: MyParser Exp
factor = (parens expr <|> number <|> variable) <* spaces

term :: MyParser Exp
term = chainl1 factor (do { op <- choice [char '*', char '/']; spaces; return (\e1 e2 -> if op == '*' then Times e1 e2 else error "divisão não implementada") })

expr :: MyParser Exp
expr = chainl1 term (do { op <- choice [char '+', char '-']; spaces; return (\e1 e2 -> if op == '+' then Plus e1 e2 else Minus e1 e2) })

rexp :: MyParser Exp
rexp = try (do
            e1 <- expr
            op <- choice [try (string ">"), try (string "<"), try (string "=")]
            spaces
            e2 <- expr
            return $ case op of
              ">" -> Greater e1 e2
              "<" -> Greater e2 e1
              "=" -> error "operador de igualdade não suportado por enquanto"
              _   -> error "operador relacional desconhecido"
           )
       <|> expr

assign :: MyParser Com
assign = do
  name <- many1 letter
  spaces
  colonEqual
  spaces
  e <- rexp
  return (Assign name e)

printE :: MyParser Com
printE = do
  printKeyword
  spaces
  e <- rexp
  return (Print e)

seqv :: MyParser Com
seqv = do
  cmds <- braces (command `sepEndBy` semicolon)
  case cmds of
    [] -> return (Print (Constant 0))
    [c] -> return c
    (c:cs) -> return (foldl Seq c cs)

cond :: MyParser Com
cond = do
  ifKeyword
  spaces
  e <- rexp
  spaces
  thenKeyword
  spaces
  c1 <- command
  spaces
  elseKeyword
  spaces
  c2 <- command
  return (Cond e c1 c2)

while :: MyParser Com
while = do
  whileKeyword
  spaces
  e <- rexp
  spaces
  doKeyword
  spaces
  c <- command
  return (While e c)

declare :: MyParser Com
declare = do
  declareKeyword
  spaces
  name <- many1 letter
  spaces
  char '='
  spaces
  e <- rexp
  spaces
  inKeyword
  spaces
  c <- command
  return (Declare name e c)

command :: MyParser Com
command = (try declare <|> try while <|> try cond <|> try assign <|> try printE <|> seqv) <* spaces

languageParser :: MyParser Com
languageParser = spaces *> command <* eof