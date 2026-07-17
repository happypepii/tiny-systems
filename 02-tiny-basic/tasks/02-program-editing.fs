// ----------------------------------------------------------------------------
// 02 - Implement interactive program editing
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  | Goto of int

type State = 
  { Program : list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  // TODO: Take 'value' of type 'Value', pattern match on it and print it nicely.
  match value with
  | StringValue s -> printf "%s" s

let getLine state line =
  // TODO: Get a line with a given number from 'state.Program' (this can fail 
  // if the line is not there.) You need this in the 'Goto' command case below.
  let exp = state.Program |> List.tryFind(fun(l, _) -> l = line)
  match exp with
  | Some(l, e) -> (l, e)
  | _ -> failwith "No such line"

let addLine state (line, cmd) = 
  // TODO: Add a given line to the program state. This should overwrite 
  // a previous line (if there is one with the same number) and also ensure
  // that state.Program is sorted by the line number.
  // HINT: Use List.filter and List.sortBy. Use F# Interactive to test them!
  failwith "not implemented"

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression expr = 
  // TODO: Implement evaluation of expressions. The function should take 
  // 'Expression' and return 'Value'. In this step, it is trivial :-)
  match expr with
  | Const v -> v

let rec runCommand state (line, cmd) =
  match cmd with 
  | Run ->
      let first = List.head state.Program    
      runCommand state first

  | Print(expr) ->
      // TODO: Evaluate the expression and print the resulting value here!
      let v  = evalExpression expr
      printValue v
      runNextLine state line
  | Goto(line) ->
      // TODO: Find the right line of the program using 'getLine' and call 
      // 'runCommand' recursively on the found line to evaluate it.
      let foundLine = getLine state line
      runCommand state foundLine

and runNextLine state line = 
  // TODO: Find a program line with the number greater than 'line' and evalaute
  // it using 'runCommand' (if found) or just return 'state' (if not found).
  let nextLine = 
    state.Program 
    |> List.filter(fun(l, _) -> l > line) 
    |> List.sortBy fst 
    |> List.tryHead
  
  match nextLine with
  | Some(l, e) -> runCommand state (l, e)
  | _ -> "next line not found"

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
  // TODO: Simulate what happens when the user enters a line of code in the 
  // interactive terminal. If the 'line' number is 'Some ln', we want to 
  // insert the line into the right location of the program (addLine); if it
  // is 'None', then we want to run it immediately. To make sure that 
  // 'runCommand' does not try to run anything afterwards, you can pass 
  // 'System.Int32.MaxValue' as the line number to it (or you could use -1
  // and handle that case specially in 'runNextLine')
  failwith "not implemented"
      

let runInputs state cmds =
  // TODO: Apply all the specified commands to the program state using 'runInput'.
  // This is a one-liner if you use 'List.fold' which has the following type:
  //   ('State -> 'T -> 'State) -> 'State -> list<'T> -> 'State
  failwith "not implemented" 

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let empty = { Program = [] }


runInputs empty helloOnce |> ignore
runInputs empty helloInf |> ignore
