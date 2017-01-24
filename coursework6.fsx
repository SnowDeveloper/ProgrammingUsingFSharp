(*

  Coursework 6: Property based testing


*)



#r @"..\packages\FsCheck.2.6.2\lib\net45\FsCheck.dll"

open FsCheck

(*
    Task 1:

    Define FsCheck properties for the following statements:

      * Taking the lengths of two lists xs and ys and adding them yields the
        same value as concatenating xs and ys and taking the length of the
        result.

      * Reversing two lists xs and ys and concatenating the results yields the
        same value as concatenating ys and xs and reversing the result.
*)

let concatenationList xs ys = List.length(xs) + List.length(ys) = List.length(xs@ys)
Check.Quick concatenationList

let reverseList xs ys = List.rev(ys)@List.rev(xs) = List.rev(xs@ys)
Check.Quick reverseList


(*
    Task 2:

    A palindrome is a list that is equal to its reverse. Below you find a
    function isPalindrome, which checks whether a given list is a palindrome.

     a) Define an FsCheck property that expresses the above definition of a
        palindrome. Use the operator ==> for defining your property.

     b) Define a variant of your property which makes FsCheck show the
        distribution of the lenghts of the lists on which the property was
        tested.
*)

let rec isPalindrome xs =
  match xs with
    | []        -> true
    | (x :: xs) -> match List.rev xs with
                     | []        -> true
                     | (y :: ys) -> x = y && isPalindrome ys


let checkPalindrome xs = isPalindrome xs ==> (xs = List.rev(xs))
Check.Quick checkPalindrome 

let checkPalindromeLength xs = Prop.collect (List.length xs) (checkPalindrome xs)
Check.Quick checkPalindromeLength


(*
    Task 3:

    Below you find a function toPalindrome, which converts a given list into a
    palindrome of the same length, more or less by replacing the second half of
    the list with the reverse of the first half.

     a) Define an FsCheck property that expresses the definition of a
        palindrome from the previous task. This time, make sure that FsCheck
        does not generate arbitrary lists from which it uses only the
        palindromes, but generates palindromes directly.

     b) Define a variant of your property which makes FsCheck show the
        distribution of the lenghts of the lists on which the property was
        tested.
*)

let toPalindrome xs =
  let len       = List.length xs
  let suffixLen = len / 2
  let prefixLen = if 2 * suffixLen = len then suffixLen else suffixLen + 1
  let take n xs = Seq.toList (Seq.take n xs)
  take prefixLen xs @ List.rev (take suffixLen xs)  

let palinGenerator = Gen.map (fun (a: int list) -> toPalindrome a) Arb.generate<list<int>>

let checkPalindromeUsingGenerator = (Prop.forAll (Arb.fromGen palinGenerator) (fun xs -> isPalindrome xs))


(*
    Task 4:

    Copy all the code into coursework6.fs file, use the appropriate [<TestFixture>],
    [<Property>] attributes so that the tests are runnable using FsCheck.NUnit (v. 2.6.2)

*)






(*  Task 5:

    Take the decision tree code from the lecture notes and write unit and property based tests
    to the extent that you consider the code reasonably well tested.

*)