(*

  Department of Computer Science
  Tallinn University of Technology
  -----------------------------------

  Coursework 1: Basic operations on lists

  ------------------------------------
  Name: Madhushree Singh
  TUT Student ID: 166807IVSM
  ------------------------------------


  Answer the questions below.  You answers to questions 1--9 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded.

  To submit the coursework you will be asked to
  
  1) Check out your empty GIT repository
  from the server git.ttu.ee using instructions on page
  https://courses.cs.ttu.ee/pages/ITT8060

  2) Put your solution into a file courswork1/coursework1.fsx
  in the repository. Commit it and push it to the server!

  NB! It is very important to make sure you use the exact name using
  only small caps. Files submitted under wrong name may not get a grade.
//-----------Upload the homework in the master repo
 
*)

// 1. Make an empty list of generic type.
let list1 = []

// 2. Make an empty list of type 'char list' (or list<char>).
let list2 = List.empty<char>

// 3. Make a three element list called 'unis' containing pairs of university
// website url (string) and year of establishment (int). The year of
// establishement should be that of the university.
let unis = [("http://www.ut.ee/en", 1632);("http://www.ttu.ee/en",1918);("http://www.harvard.edu/",1636)]

// 4. Write a function filterOutYoungerThan: int -> string * int -> string * int to filter out news sources
// which are less than some integer years old.  It should use the List.filter
// function from the library.
//let filterOutYoungerThan filteredList = unis |> List.filter (fun (url,yr) -> yr <= 1900) filteredList
let filterOutYoungerThan filteredList age : (string*int) list = List.filter (fun (url,yr) -> yr < 2016-age) filteredList

 
// 5. Test the function 'filterOutYoungerThan' to filter out universities younger than 100 years in 
// your list 'unis'.
filterOutYoungerThan unis 100

// 6. Calculuate the average age of your list of universities. The
// function should use pattern matching and recursion.

let sum list =
   let rec loop list acc =
       match list with
       | (_,head) :: tail -> loop tail (acc + head)
       | [] -> acc
   loop list 0
let avg list = sum list / list.Length
avg unis


// 7. Using the http function write a function
//
//    getSource : (string * int) -> (string * string)
//
//    which takes a pair of a url and a year of establishment of the university and
//    returns a pair of a url and the html source of the page.
open System.Net
open System
open System.IO

let getSource (url: string, year: int) =    
    let req = WebRequest.Create(Uri(url))
    let resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new IO.StreamReader(stream)
    let html = reader.ReadToEnd()        
    url, html

let valURL, valHTML = getSource unis.[0]

// 8. Write a function
//
//    getSize : (string * string) -> (string * int)
//    
//    which takes a pair of a url and its html source and returns a
//    pair of the url and the size of the html (length of the string).
let getSize (url: string, html: string) = 
    url, String.length(html)

getSize (getSource(unis.[0]))


// 9. Write a function
//
//    getSourceSizes : (string * int) list -> (string * int) list
//
//    It should take a list of pairs of urls and years of
//    establishment and return a list of pairs of urls and
//    corresponding html source sizes
(*open System.Net
open System
open System.IO

let rec getSourceSizes (urls: string, year: int) mList= 
    match mList with
        |   _,_ ->
            let url = fst mList;
            let req = WebRequest.Create(Uri(url))
            let resp = req.GetResponse()
            let stream = resp.GetResponseStream()
            let reader = new IO.StreamReader(stream)
            let html = reader.ReadToEnd()        
            url, String.length(html)

let unis = [("http://www.ut.ee/en", 1632);("http://www.ttu.ee/en",1918);("http://www.harvard.edu/",1636)];;
getSourceSizes unis*)

let rec getSourceSize mList : (string * int)list =
    match mList with
    | [] -> []
    | h::t -> getSize(getSource(h)) :: getSourceSize t

getSourceSize unis