(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: Higher order functions, option, list

  ------------------------------------
  Name: Madhushree Singh
  Student ID: 166807IVSM
  ------------------------------------


  Answer the questions below. You answers to the questions should be
  correct F# code written after the question. This file is an F# script
  file; it should be possible to load the whole file at once. If you
  can't, then you have introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your solution to the repository as file
  coursework4.fsx in directory coursework4.

  The deadline for completing the above procedure is Friday,
  October 21, 2016.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

*)

// 1. Write a function by pattern matching
// 
//   flattenOption : option<option<'a>> -> option<'a>
//
//   which squashes two layers of possible successes or failures into 1
//   E.g. Some Some 1 -> Some 1
let flattenOption optn =
    match optn with
    |(Some (Some a)) -> Some a
    |Some(None)      -> None
    |None            -> None

flattenOption (None)
flattenOption(Some(Some 1))


// 2. Can flattenOption by implemented using bind? If so, do it!
let bindflattenOption optn = optn |> Option.bind ( fun a -> a) 

bindflattenOption (None)
bindflattenOption (Some(Some 1))



// 3. Write a function
//
//    idealist : list<option<'a>> -> list<'a>
//
//    which collects a list of possible successes or failures into a
//    list containing only the successes. Pay close attention to the type.
let rec idealist (items: list<option<'a>>) = 
    match items with
    | [] -> []
    | (Some hd) :: t1 -> hd :: idealist t1
    | None :: t1 -> idealist t1

let list1 = [Some 3; Some 5; None; Some 8; None]
let list2 = [Some 5; Some 4; Some 11; Some 6]
idealist list1
idealist list2

// 4. Write a function
//
//    conservative : list<option<'a>> -> option<list<'a>>
//
//    that takes a list of possible successes or failures and returns
//    a list of successes if everything succeeded or returns failure
//    if 1 or more elements of the list was a failure. Again, pay
//    close attention to the type.
let conservative (elem) =
    let filterElem = idealist elem
    if elem.Length = filterElem.Length then Some filterElem
    else None

let list5 = [Some 3; Some 5; None; Some 8; None]
let list6 = [Some 5; Some 4; Some 11; Some 6]
let list7 = [Some 3; Some 5; None; Some 8]
conservative list5
conservative list6
conservative list7

// 5. Write a function
//
//    chars : list<string> -> list<char>
//
//    This function should use List.collect (bind) and have the
//    following behaviour:
//    ["hello";"world"] -> ['h';'e';'l';'l';'o';'w';'o';'r';'l';'d']

let chars (ch:list<string>) : list<char> = ch |> List.collect(fun x ->  (x |> List.ofSeq))

let list3 = ["hello";"world"]
chars list3


// 6. Write a function
//
//    iprint : list<int> -> string
//
//    This function should use List.foldBack and have the following behaviour:
//    [1 .. 5] |-> "1,2,3,4,5,"

let iprint (list: int list)  = List.foldBack (fun elem acc ->  elem.ToString() + "," + acc) list ("")

let list4 = [1..5]
iprint list4 


