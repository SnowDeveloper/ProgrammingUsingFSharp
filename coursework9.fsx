(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------------------------------------

  Coursework 9: Asynchronous and reactive programming

  ------------------------------------------------------------------------------
  Name: Madhushree Singh
  Student ID: 166807IVSM
  ------------------------------------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework9.fsx in directory coursework9.

  Please do not upload DLL-s. Just include a readme.txt file containing the
  dependencies required (additional DLLs)

  The deadline for completing the above procedure is Friday, December 9, 2016.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)



(*
  Task 1:

  Write a function downloadParallel : string list -> Async<string []> that takes
  a list of URLs and downloads the resources referenced by these URLs in
  parallel. Use the function downloadAsync from the lecture in your
  implementation.
*)
open System.IO
open System.Net

let readToEndAsync (reader : StreamReader) =
  Async.AwaitTask (reader.ReadToEndAsync())

let downloadAsync (url : string) =
  async {
    let  request  = HttpWebRequest.Create(url)
    use! response = request.AsyncGetResponse()
    let  stream   = response.GetResponseStream()
    use  reader   = new StreamReader(stream)
    return! readToEndAsync reader
  }


let downloadParallel (strList: string list) =
    (Seq.map (fun xs -> downloadAsync xs) (List.ofSeq strList)) |> Async.Parallel

let download = downloadParallel ["http://tut.ee/";"http://ut.ee/"; "http://tlu.ee/"]
let dlResult = Async.RunSynchronously download 

(*
  Task 2:

  Write a function downloadSemiParallel : string list -> Async<string []> that
  takes a list of URLs and downloads the resources referenced by these URLs.
  Resources from URLs with the same domain name shall be downloaded
  sequentially, but otherwise, parallelism shall be used. The order of the
  resources in the resulting array can be chosen by you.
*)
let downloadSemiParallel (strList: string list) =
    strList |> List.groupBy (fun x -> System.Uri(x).Host)
        |> List.collect (fun (_,ls) -> List.map (fun url -> downloadAsync url) ls)
        |> Async.Parallel


downloadSemiParallel ["http://tut.ee/";"http://ut.ee/"; "http://tut.ee/kooliopilasele/"; "http://www.ox.ac.uk/"]


(*
  Task 3:

  Write an event stream additions : IObservable<string> that emits an event
  everytime a file is created in the current directory. Each such event shall
  carry the name of the created file.

  Furthermore, write an event stream removals : IObservable<string> that emits
  an event everytime a file is removed from the current directory. Each such
  event shall carry the name of the removed file.
*)
let watcher = new FileSystemWatcher(__SOURCE_DIRECTORY__)
watcher.EnableRaisingEvents <- true
    

let additions = watcher.Created |> Observable.map (fun args -> sprintf "%s\n  successfully created" args.Name)

let removals = watcher.Deleted |> Observable.map (fun args -> sprintf "%s\n  successfully removed" args.Name)

let reactToEvents () = 
     Observable.add (printfn "%s") removals
     Observable.add (printfn "%s") additions

reactToEvents ()



(*
  Task 4:

  Below you find the definition of a type Change whose values represent changes
  to a directory. Use the event streams from Task 3 to define an event stream
  changes : IObservable<Change> of all file additions and removals in the
  current directory.
*)

type Change =
  | Addition of string
  | Removal  of string

let removalChanges = watcher.Deleted |> Observable.map (fun args -> Removal (sprintf "%s\n  successfully removed" args.Name))

let additionChanges = watcher.Created |> Observable.map (fun args -> Addition (sprintf "%s\n  successfully added" args.Name))

let changes = Observable.merge additionChanges removalChanges


(*
  Task 5:

  Use the event stream changes from Task 4 to define an event stream
  turnover : IObservable<int> that tells at every file addition or removal how
  much the number of files in this directory has increased since the beginning
  (with negative numbers signifying a decrease). For example, if two files are
  added and one file is removed afterwards, there should be three events, that
  carry the numbers 1, 2, and 1, respectively.
*)

let trackChanges count ch = 
    match ch with
    | Addition _ -> count + 1
    | Removal _ -> count - 1
    
let turnover = Observable.scan trackChanges 0 changes

let countTurnovers () = Observable.add (printfn "%d") turnover

countTurnovers()
