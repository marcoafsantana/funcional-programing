# Assignment 2 - Functional Programming

**Author:**

  * Marco Aurélio Ferreira Santana - MAFS3


## About This Assignment

This repository contains the solutions for the **second assignment list** from the Functional Programming course (IF708) at the Centro de Informática (CIn/UFPE). The main focus of this assignment is the use of the **QuickCheck** library to perform property-based testing, validating the correctness of functions and algebraic data types.

The exercises cover:

  * Proving properties of the list concatenation operator (`++`).
  * Analyzing functions and testing a Binary Search Tree.
  * Validating mathematical properties of a `Set` type.
  * Creating custom QuickCheck generators for an `Expr` type and testing its algebraic properties.

## Exercises and Implementations

### 1\. Concatenation Properties (`questao1.hs`)

This exercise consists of proving, via QuickCheck, two fundamental laws of the `++` operator for finite lists. The `questao1.hs` file implements tests for the following properties:

  * **Right Identity:** `xs ++ [] = xs`
  * **Associativity:** `xs ++ (ys ++ zs) = (xs ++ ys) ++ zs`

### 2\. Analysis of `take` and Binary Search Tree Tests (`questao2.hs`)

This exercise has two parts: a theoretical analysis of two implementations of the `take` function and the definition of properties for a Binary Search Tree (BST). The `questao2.hs` file contains:

  * An analysis of the strictness of the `take1` and `take2` functions.
  * Three QuickCheck properties to validate the BST implementation:
    1.  After an element is inserted, it must be a member of the tree.
    2.  Converting the tree to a list must result in a sorted list with no duplicates.
    3.  Inserting a list of elements into the tree and then converting it back to a list should be equivalent to sorting the original list and removing duplicates.

### 3\. Set Properties (`questao3.hs`)

This exercise requires defining five QuickCheck properties that reflect the mathematical laws of sets, using the `Set.hs` module. The `questao3.hs` file implements the following properties:

1.  **Union Identity:** The union of a set with an empty set is the set itself.
2.  **Intersection Commutativity:** `A ∩ B = B ∩ A`.
3.  **Relation between Difference and Intersection:** `A \ B = A \ (A ∩ B)`.
4.  **Intersection Associativity:** `A ∩ (B ∩ C) = (A ∩ B) ∩ C`.
5.  **Relation between Subset and Union:** If A is a subset of B, then `A U B = B`.

### 4\. Generator and Tests for Expressions (`questao4.hs`)

The final exercise focuses on an `Expr` data type for arithmetic expressions. The `questao4.hs` file contains:

  * A custom QuickCheck generator, `genExpr`, which creates expressions of a limited size.
  * An `Arbitrary` instance for the `Expr` type, using `genExpr`.
  * Five QuickCheck properties to test the correctness of an `eval` function, based on algebraic laws such as distributivity, commutativity, and associativity.

## How to Run

### Requirements:

  * [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/) installed.
  * **QuickCheck** library. To install it, run:
    ```bash
    cabal install QuickCheck
    ```

### Running the Files:

Each exercise can be compiled and run individually.

  * **Question 1:**

    ```bash
    ghc questao1.hs && ./questao1
    ```

  * **Question 2:**

    ```bash
    ghc questao2.hs && ./questao2
    ```

  * **Question 3 (depends on Set.hs):**

    ```bash
    ghc questao3.hs Set.hs && ./questao3
    ```

  * **Question 4:**

    ```bash
    ghc questao4.hs && ./questao4
    ```