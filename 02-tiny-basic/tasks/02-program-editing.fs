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
  match value with
  | StringValue s -> printfn "%s" s

let getLine state line = 
  match List.tryFind (fun (l, _) -> l = line) state.Program with
  | Some (_, cmd) -> cmd
  | None -> failwith "Line %d not found" line

let addLine state (line, cmd) = 
  // TODO: Add a given line to the program state. This should overwrite 
  // a previous line (if there is one with the same number) and also ensure
  // that state.Program is sorted by the line number.
  // HINT: Use List.filter and List.sortBy. Use F# Interactive to test them!
  {
    state with 
      Program = 
        (state.Program 
        |> List.filter (fun(l, _) -> l <> line )) // keep everything except for the line that has the same number as l
        @ [line, cmd]
        |> List.sortBy fst
  }

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression expr = 
  match expr with
    | Const v -> v

let rec runCommand state (line, cmd) =
  match cmd with 
  | Print expr ->
      let v = evalExpression expr
      printValue v
      runNextLine state line
  | Run ->
      let first = List.head state.Program    
      runCommand state first
  | Goto line ->
      let c = getLine state line
      runCommand state (line, c)

and runNextLine state line = 
  let next = 
    state.Program
    |> List.filter (fun(l, _) -> l > line)
    |> List.sortBy fst
    |> List.tryHead
  match next with
    | Some (nextLine, cmd) ->
      runCommand state (nextLine, cmd)
    | None ->
      state

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
  match line with
  | Some ln -> 
    addLine state (ln, cmd)
  | None -> runCommand state (System.Int32.MaxValue, cmd)
      

let runInputs state cmds =
  // TODO: Apply all the specified commands to the program state using 'runInput'.
  // This is a one-liner if you use 'List.fold' which has the following type:
  //   ('State -> 'T -> 'State) -> 'State -> list<'T> -> 'State
  List.fold runInput state cmds 

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
