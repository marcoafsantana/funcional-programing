# Assingment 3 - Functional Programming

Marco Aurélio Ferreira Santana - MAFS3
Arthur Henrique Costa - AHTLC

## Exp Interpreter:

Follow `especificacaoInterpretador.pdf` to built a Expression Interpreter

## Setup:

### Requirements:
- GHC and Cabal installed

### Install Dependecies and run project:
- Install dependencies:
`cabal install parsec`
- Compile:
`ghc Main.hs back.hs front.hs`
- Run:
`./Main`


## Examples:

1. X + 5
    a. Input: `declare x = 10 in {x := x + 5; print x}`
    b. Output: `15`
1. X + 5
    a. Input:
    ```
        declare n = 5 in declare result = 1 in {
            while n > 0 do {
                result := result * n;
                n := n - 1
            };
            print result
        }
    ```
    b. Output: `120`