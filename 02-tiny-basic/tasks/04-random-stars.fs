// ----------------------------------------------------------------------------
// 04 - Random function and (not quite correct) POKE
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  // NOTE: Clear clears the screen and Poke(x, y, e) puts a string 'e' at 
  // the console location (x, y). In C64, the actual POKE writes to a given
  // memory location, but we only use it for screen access here.
  | Clear
  | Poke of Expression * Expression * Expression

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    // TODO: You will need to include random number generator in the state!
    }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  match value with
  | StringValue s -> printf "%s" s
  | NumberValue n -> printf "%d\n" n
  | BoolValue b -> if b then printf "true\n" else printf "false\n"

let getLine state line =
  let exp = state.Program |> List.tryFind(fun(l, _) -> l = line)
  match exp with
  | Some(l, e) -> (l, e)
  | _ -> failwith "No such line"

let addLine state (line, cmd) = 
  let filtered = state.Program |> List.filter (fun (l, _) -> l <> line)
  let newList = (line, cmd) :: filtered |> List.sortBy fst
  { state with Program = newList }

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

// NOTE: Helper function that makes it easier to implement '>' and '<' operators
// (takes a function 'int -> int -> bool' and "lifts" it into 'Value -> Value -> Value')
let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let rec evalExpression state expr = 
  match expr with
  | Const v -> v
  | Function (fname, args) ->
    let evaluatedArgs = args |> List.map (evalExpression state)

    match fname, evaluatedArgs with
    | "-", [NumberValue a; NumberValue b] -> NumberValue(a - b)
    | "=", [a; b] -> BoolValue(a = b)
    | _ -> failwith "unsupported function"
  | Variable s -> 
    match Map.tryFind s state.Variables with
    | Some v -> v
    | None -> failwith ("Undefined variable: " + s)

let rec runCommand state (line, cmd) =
  match cmd with 
  | Run ->
      let first = List.head state.Program    
      runCommand state first

  | Print(expr) ->
    let v  = evalExpression state expr
      printValue v
      runNextLine state line
  | Goto(line) ->
      let foundLine = getLine state line
      runCommand state foundLine

  | Assign (var, expr) ->
    let v = evalExpression state expr
    let newVars = Map.add var v state.Variables
    let newState = { state with Variables = newVars }
    runNextLine newState line

  | If (cond, cmd) ->
    let vc = evalExpression state cond
    match vc with
    | BoolValue true -> runCommand state (line, cmd)
    | _ -> runNextLine state line
  
  // TODO: Implement two commands for screen manipulation
  | Clear | Poke _ -> failwith "not implemented"

and runNextLine state line = 
  let nextLine = 
    state.Program 
    |> List.filter(fun(l, _) -> l > line) 
    |> List.sortBy fst 
    |> List.tryHead
  
  match nextLine with
  | Some(l, e) -> runCommand state (l, e)
  | None -> state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) = 
  match line with
  | Some ln -> addLine state (ln, cmd) 
  | None -> runCommand state (System.Int32.MaxValue, cmd)

let runInputs state cmds =
  List.fold runInput state cmds

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// NOTE: Writing all the BASIC expressions is quite tedious, so this is a 
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.: 
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or 
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty } // TODO: Add random number generator!

// NOTE: Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars = 
  [ Some 10, Clear
    Some 20, Poke("RND" @ [num 60], "RND" @ [num 20], str "*")
    Some 30, Assign("I", num 100)
    Some 40, Poke("RND" @ [num 60], "RND" @ [num 20], str " ")
    Some 50, Assign("I", var "I" .- num 1)
    Some 60, If(var "I" .> num 1, Goto(40)) 
    Some 100, Goto(20)
    None, Run
  ]

// NOTE: Make the cursor invisible to get a nicer stars animation
System.Console.CursorVisible <- false
runInputs empty stars |> ignore
