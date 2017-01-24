(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 7: Tail recursion and laziness

  ------------------------------------------------
  Name: Madhushree Singh
  Student ID: 166807IVSM
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework7.fsx in directory coursework7.

  Please do not upload DLL-s. Just include a readme.txt file containing the
  dependencies required (additional DLLs)

  The deadline for completing the above procedure is Friday, November 18, 2016.

  We will consider the submission to be latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Write a function maxInList : int list -> int that returns the maximum element
  in the given list. Make sure your implementation uses tail recursion.
*)
let maxInList (myList: int list) = 
    let rec findMax1 ls acc =
        match ls with
        | [] -> acc
        | x::xs -> 
            if x > acc then findMax1 xs x
            else findMax1 xs acc
    findMax1 myList 0

maxInList [-1] // Wrong result

maxInList [2 .. 2000000]
(*
  Task 2:

  Write a function reverse :: 'a list -> 'a list that works like the function
  List.rev. Make sure your implementation uses tail recursion.
*)
let reverse myList = 
    let rec reverseList ls acc =
        match ls with
        | [] -> acc
        | x :: xs -> reverseList xs (x::acc)
    reverseList myList []

reverse [11 .. 20]

(*
  Task 3:

  Below you find the definition of a type Tree of leaf-labeled trees. Write a
  function maxInTree : int Tree -> int that returns the maximum label in the
  given tree. Use continuation-passing style in your implementation.
*)

type 'a Tree =
  | Leaf   of 'a
  | Branch of 'a Tree * 'a Tree

let maxInTree (tree: int Tree) = 
    let rec findMax2 tree cont = 
        match tree with
          | Leaf x               -> cont x
          | Branch (left, right) -> findMax2 left  (fun leftside  ->
                                    findMax2 right (fun rightside ->
                                    cont (max leftside rightside)))
    findMax2 tree id


(*
  Task 4:

  Write a function maxInTree' : int Tree -> int that returns the maximum label
  in the given tree, like the function maxInTree from Task 3 does. Use
  continuation-passing style in combination with accumulation in your
  implementation.
*)

// Wrong name!
let maxInTreeAcc (tree: int Tree) = 
    let rec findMax3 acc tree cont = 
        match tree with
          | Leaf x              -> cont (max acc x)
          | Branch (left, right) -> findMax3 acc left  (fun acc ->
                                    findMax3 acc right cont)
    findMax3 0 tree id

maxInTreeAcc (Leaf -1) // Wrong result

(*
  Task 5:

  The function streamMap : ('a -> 'b) -> 'a Stream -> 'b Stream from the lecture
  is the stream analog of the function List.map. Write a function streamFilter :
  ('a -> bool) -> 'a Stream -> 'a Stream that is the stream analog of the
  function List.filter.
*)

type 'a Stream =
  | Stream of 'a * Lazy<'a Stream>

let streamTail xs =
  match xs with
    | Stream (_, lxs) -> lxs.Value

let rec from n = Stream (n, lazy from (n + 1))

let rec streamFilter f xs = 
    match xs with
        | Stream (x, lxs) -> if f x then Stream (x, lazy streamFilter f lxs.Value)
                             else streamFilter f lxs.Value
                             
let allevens = streamFilter (fun n -> n%2 = 0) (from 1)
streamTail allevens
