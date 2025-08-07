# Assignment 1 - Functional Programming

**Author:**

* Marco Aurélio Santana - MAFS3

## About the Assignment

This repository contains the solutions for the **first assignment list** from the Functional Programming course (IF701) at the Centro de Informática (CIn/UFPE). The assignment covers two main problems:

1.  **Caesar Cipher:** Implementation of encoding and decoding functions using the chi-squared test to break the cipher.
2.  **Tautology Checker:** Development of a function that evaluates whether a logical proposition is always true.

---

## Exercises and Implementations

### 1. Caesar Cipher

The `cesar.hs` file implements an encoder and a decoder for the Caesar Cipher.

* **Encoding:** The `encode` function shifts the letters of a string according to an integer factor.
* **Decoding:** The `crack` function analyzes the character frequency in the encoded string. It compares this frequency with a standard English language frequency table using the chi-squared test ($\chi^2$) to determine the most likely shift factor and thus decode the message.

### 2. Tautology Checker

The `tautology.hs` file implements a tautology checker for a set of logical propositions.

* **Structure:** An algebraic data type `Prop` has been defined to represent propositions, including constants, variables, negation, conjunction, and implication.
* **Evaluation:** The `isTaut` function checks if a proposition is a tautology. To do this, it generates all logical value combinations (true/false) for the proposition's variables and evaluates if the result is always true.

---

## How to Run

### Requirements:

* [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/) installed.

### Running the Files:

You can compile and run each file individually.

**For the Caesar Cipher:**

```bash
# Compile the program
ghc cesar.hs

# Run the program
./cesar