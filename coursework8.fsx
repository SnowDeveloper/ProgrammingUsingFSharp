(*

  Coursework 8: Sequences and computation expressions


*)

(*
  Task 1:

  Define a sequence powers : int seq that contains all powers of 2 in ascending
  order. Use Seq.unfold in your implementation.
*)

let powers =
  let power1 a =
    Some (a, a * 2)
  Seq.unfold power1 2

powers

(*
  Task 2:

  Define a sequence primes : int seq that contains all prime numbers in
  ascending order. Use sequence expressions in your implementation. You may want
  to use the function isPrime : int -> bool defined below. This function checks
  whether any given number that is greater or equal 2 is a prime number.
*)

let isPrime n =
  let rec hasDivisorFrom d n =
    if d * d <= n then
      if n % d = 0 then
        true
      else
        hasDivisorFrom (d + 1) n
    else
      false
  not (hasDivisorFrom 2 n)


let primes =
  let rec x num =
    seq {
        if isPrime num then 
            yield num
        yield! x (num+1)
    }
  x 2 

primes



(*
  Task 3:

  Define a sequence primes' : int seq that again contains all prime numbers in
  ascending order. This time, do not use sequence expressions in your
  implementation, but use an appropriate function from the Seq module. Again,
  you may want to use the function isPrime : int -> bool defined above.
*)
let rec from z =
  seq {
    yield z
    yield! from (z + 1)
  }
let nat_nums = from 2
let primes' = Seq.filter isPrime nat_nums

primes'



(*
  Task 4:

  Define a function fourthRoot : float -> float option that returns Some x if x
  is the 4th root of the argument, and None if the argument has no 4thÂ root. In
  your implementation, use the squareRoot function from the lecture and
  computation expressions for the option type as defined in the lecture.
*)

type OptionBuilder () =
  member this.Bind   (opt, f) = Option.bind f opt
  member this.Return x        = Some x

let option = new OptionBuilder ()

let squareRoot x =
  if x >= 0.0 then Some (sqrt x) else None

let fourthRoot x = 
    option {
        let! temp = squareRoot x
        return squareRoot temp
    }
fourthRoot 625.0


(*
  Task 5:

  A function from a type 'env to a type 'a can be seen as a computation that
  computes a value of type 'a based on an environment of type 'env. We call such
  a computation a reader computation, since compared to ordinary computations,
  it can read the given environment. Below you find the following:

    â€¢ the definition of a builder that lets you express reader computations
      using computation expressions

    â€¢ the definition of a reader computation ask : 'env -> 'env that returns the
      environment

    â€¢ the definition of a function runReader : ('env -> 'a) -> 'env -> 'a that
      runs a reader computation on a given environment

    â€¢ the definition of a type Expr of arithmetic expressions

  Implement a function eval : Expr -> map <string, int> -> int that evaluates
  an expression using an environment which maps identifiers to values. Use
  computation expressions for reader computations in your implementation. Note
  that partially applying eval to just an expression will yield a function of
  type map <string, int> -> int, which can be considered a reader computation.
  This observation is the key to using computation expressions.
*)

type ReaderBuilder () =
  member this.Bind   (reader, f) = fun env -> f (reader env) env
  member this.Return x           = fun _   -> x

let reader = new ReaderBuilder ()

let ask = id

let runReader = (<|)

type Expr =
  | Const of int
  | Ident of string
  | Neg   of Expr
  | Sum   of Expr * Expr
  | Diff  of Expr * Expr
  | Prod  of Expr * Expr
  | Let   of string * Expr * Expr


let rec partialEval tree env =     
    match tree with
    | Const n       -> n
    | Ident s       -> Map.find s env
    | Neg t       -> - (partialEval t env)
    | Sum (t1,t2)   -> partialEval t1 env + partialEval t2 env
    | Diff (t1, t2) -> partialEval t1 env - partialEval t2 env
    | Prod (t1,t2)  -> partialEval t1 env * partialEval t2 env
    | Let (s,t1,t2) -> let v1 = partialEval t1 env
                       let env1 = Map.add s v1 env
                       partialEval t2 env1

let eval expTree = 
    reader {
        let! x = partialEval expTree
        return x
    }
    

let expr2 = Prod(Ident "a",
                Sum (Neg (Const 3), 
                    Let ("x", Const 5, Sum(Ident "x", Ident "a"))
                    )
                )
let envGood = Map.add "a" -7 Map.empty
eval expr2 <| envGood