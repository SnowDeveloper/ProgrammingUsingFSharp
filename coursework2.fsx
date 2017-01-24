(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists, recursion

  ------------------------------------
  Name: Madhushree Singh
  TUT Student ID: 166807IVSM
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by September 30, 2016.
*)

// 1. Make a value sl containing empty list of type string list.
let s1 = List.empty<string>


// 2. Make a function shuffle: int list -> int list that rearranges the elements of the argument list
// in such a way that the first value goes to first, last to second,
// second to third, last but one to fourth etc.
// E.g.
// shuffle [] -> []
// shuffle [1;2] -> [1;2]
// shuffle [1..4] -> [1;4;2;3]

let rec rev (input:int list)=
    match input with 
    | [] -> []
    | h :: t -> rev(t) @ [h]

let rec shuffle (list:int list)=
    match list with
    | [] -> []
    | h :: t -> h :: shuffle (rev(t))

shuffle [1..4]


// 3. Make a function segments: int list -> int list list that splits the list passed
// as an argument into list of lists of nondecreasing segments.
// The segments need to be of maximal possible length (the number of segments
// needs to be minimal)
// E.g.
// segments [] ->  []
// segments [1] -> [[1]]
// segments [3;4;5;5;1;2;3] -> [[3;4;5;5];[1;2;3]]
// returns a single run and the remainder of the input

//Reference: Modified and changed according to the question from the online resource : http://codereview.stackexchange.com/questions/40013/functional-approach-to-splitting-list-into-sub-lists
let rec div (list:int list) =
    match list with
    | [] -> [],[]
    | [h] -> [h], []
    | h :: t -> if h <= t.Head then 
                     let r, rem = div t
                     h :: r, rem
                 else [h], t

let rec segments (list:int list) =
    match div list with
    | r, []  -> [r]//
    | r, rem -> r :: segments rem//add value to the list

let a = segments [3;4;5;5;1;2;3] 

// 4. Make a function sumSublists : int list list -> int list that will compute the sums of sublists in a list of list of ints.
// Hint: use the function List.fold to compute the sums of lists of ints.
let rec sumSublists (lists : int list list) : int list = 
    match lists with
    | [] -> []
    | h :: t -> List.fold (+) 0 h :: sumSublists t

let b = sumSublists [[1;3;4;4];[1;3]]


// 5. Write a function filterSegments : (int list -> bool) -> int list list -> int list list that will filter out lists of ints
// based on some filter function. Write a filter function for even sum, odd sum, even number of elements, odd number of elements.


let evenNumCount listToFilter = 
    List.length(listToFilter) % 2 <> 0
    
let evenSumFilter listToFilter = 
    let total = List.fold (+) 0 listToFilter
    (total % 2) <> 0

let oddNumCount listToFilter = 
    List.length(listToFilter) % 2 <> 1

let oddSumFilter listToFilter = 
    let total = List.fold (+) 0 listToFilter
    (total % 2) <> 1

let filterSegments (func: int list -> bool) (listOfList: int list list) =
  List.filter func listOfList

filterSegments evenSumFilter [[3;5];[5;4];[8;2]]
