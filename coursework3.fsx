(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 3: User defined types

  ------------------------------------
  Name: Madhushree Singh
  TUT Student ID: 166807IVSM
  ------------------------------------


  Answer the questions below.  You answers to questions 1--7 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by October 9, 2015.
*)

// 1. Consider expression trees of type ExprTree declared in the lecture.
// Extend the type with if-then-else expression of the form:
// if b then e1 else e2
// where b is a boolean expression and e1 and e2 are expressions.
// An example expression could be:
// if a+3 > b+c && a>0 then c+d else e

type ExprTree = | Const  of int
                | Ident of string
                | Minus  of ExprTree
                | Sum    of ExprTree * ExprTree
                | Diff   of ExprTree * ExprTree
                | Prod   of ExprTree * ExprTree
                | Let    of string * ExprTree * ExprTree
                | IfThenElse of bool * ExprTree * ExprTree

(*
  FEEDBACK:

    You do not allow arbitrary boolean expressions as conditions, only boolean
    constants.
*)

// 2. Extend the function eval defined in the lecture to support the
// if-then-else expressions defined in Q1.

let rec eval t env =
    match t with
    | Const n        -> n
    | Ident s        -> Map.find s env
    | Minus t        -> - (eval t env)
    | Sum (t1,t2)    -> eval t1 env + eval t2 env
    | Diff (t1,t2)   -> eval t1 env - eval t2 env
    | Prod (t1,t2)   -> eval t1 env * eval t2 env
    | Let (s,t1,t2)  -> let v1 = eval t1 env
                        let env1 = Map.add s v1 env
                        eval t2 env1
    | IfThenElse (b, t1, t2) -> if b then eval t1 env
                                else eval t2 env

(*
  FEEDBACK:

    For the errornous definition of ExprTree, this is correct.
*)

// 3-4: Given the type definition:
type BList =
  | BEmpty
  | Snoc of BList * int

// 3. Make the function filterB: (prop: int -> bool) BList -> BList that will return a list for the elements of which
// the function prop returns true.
let prop a = (=) 1

let rec filterB (prop: int -> bool) (list:BList) =
    match list with
    |BEmpty -> BEmpty
    |Snoc (head,tail) -> let filterHead = filterB prop head
                         if prop tail
                         then Snoc (filterHead, tail)
                         else filterHead

(*
  FEEDBACK:

    OK
*)

// 4. Make the function mapB: (trans: int -> int) BList -> BList that will return a list where the function trans has
// been applied to each element.
let rec mapB (trans: int -> int) list:BList = 
    match list with
    |BEmpty -> BEmpty
    |Snoc (tl,h) -> Snoc((mapB trans tl), trans h)

(*
  FEEDBACK:

    OK
*)

// 5-7. Given the type definition
type Tree =
  | Nil
  | Branch2 of Tree * int * Tree
  | Branch3 of Tree * int * Tree * int * Tree

// 5. Define the value exampleTree : Tree that represents the following
//    tree:
//
//        2
//       / \
//      *  3 5
//        / | \
//       *  *  *
let tree : Tree = Nil
let thisTree = Branch2(Nil,2,Branch3(Nil,3,Nil,5,Nil))

(*
  FEEDBACK:

    OK
*)

// 6. Define a function sumTree : Tree -> int that computes the sum of
//    all labels in the given tree.
let rec sumTree tree = 
    match tree with
    |Nil -> 0
    |Branch2(t1,itm,t2) -> itm + sumTree t1 + sumTree t2
    |Branch3(t1,itm1,t2,itm2,t3) -> itm1 + itm2 + sumTree t1 + sumTree t2 + sumTree t3

(*
  FEEDBACK:

    OK
*)

// 7. Define a function productTree : Tree -> int that computes the
//    product of all labels in the given tree. If this function
//    encounters a label 0, it shall not look at any further labels, but
//    return 0 right away.
let rec productTree tree =
    match tree with
    |Nil -> 1
    |Branch2(t1,itm,t2) -> if (itm <> 0) then itm * productTree t1 * productTree t2 else 0
    |Branch3(t1,itm1,t2,itm2,t3) -> if(itm1 <> 0 && itm2 <> 0) then itm1 * itm2 * productTree t1 * productTree t2 * productTree t3 else 0

(*
  FEEDBACK:

    While your implementation does not look at the subtrees if it encounters a
    label 0, it still looks at all other remaining parts of the tree, which it
    should not do.
*)

// ** Bonus questions **

// 8. Extend the ExprTree type with a pattern match expression
// match p with [p1, ex1; p2,ex2 ...]

// 9. Extend the eval function to support match expressions.
