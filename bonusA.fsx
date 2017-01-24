// Here are some optional exercises, answer the questions below and
// make sure to write tests.

// 1. implement addition, multiplication, subtraction for Nat as
//    custom operators

// 2. Write a converstion function from Nat to int

// 3. Write an evaluator for the following language of aritmetic expressions:

type Exp =
  | Val of Nat
  | Add of Exp * Exp

//    eval : Exp -> int

// 4. Extend the language and the evaluator to support Sub, and Mult

// 5. Write an evaluator for this language which has variables as well.

type Exp<'t> =
  | Val of Nat
  | Var of 't
  | Add of Exp<'t> * Exp<'t>

//    The evaluator should take an lookup function too:
//    eval : ('t -> int) -> Exp<'t> -> int

// 6. Write a map function for Exp<'t>, it can be thought of a
//    'renaming' function that renames variables.

// 7. Write a bind function (see section 6.8.2) for Exp<'t>, it can be
//    thought of as a substitution function that replaces variables with
//    expressions.