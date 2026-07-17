// ----------------------------------------------------------------------------
// 06 - Add support for more elegant programs with GOSUB
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
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  | Clear
  | Poke of Expression * Expression * Expression
  | Print of Expression list
  | Input of string 
  | Stop
  // NOTE: Add the GOSUB jump and RETURN commands
  | GoSub of int
  | Return

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Random : System.Random 
    // TODO: Add a stack of line numbers to return to (list<int>)
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
    | "RND", [NumberValue n] ->
      NumberValue(state.Random.Next(n))
    | ">", args -> binaryRelOp (>) args
    | "<", args -> binaryRelOp (<) args
    | "||", [BoolValue a; BoolValue b] -> BoolValue(a || b)
    | "MIN", [NumberValue a; NumberValue b] -> if a < b then NumberValue a else NumberValue b
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

  | Print(exprList) ->
    exprList |> List.iter (fun e -> evalExpression state e |> printValue)
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
  
  | Clear ->
    System.Console.Clear()
    runNextLine state line
  | Poke(xExpr, yExpr, valExpr) ->
    let x = evalExpression state xExpr
    let y = evalExpression state yExpr
    let v = evalExpression state valExpr
    match x, y, v with
      | NumberValue nx, NumberValue ny, StringValue str ->
          try
              System.Console.SetCursorPosition(int nx, int ny)
              System.Console.Write(str)
          with _ -> () // ignore if out of range
          runNextLine state line
      | _ -> failwith "Poke expects (Number, Number, String)"
  | Input(varName) ->
    let inputVal = System.Console.ReadLine()
    let newVars = Map.add varName (NumberValue (int inputVal)) state.Variables
    runNextLine { state with Variables = newVars } line
  | Stop -> state
  // TODO: GOSUB needs to store the current line number on the stack for
  // RETURN (before behaving as GOTO); RETURN pops a line number from the
  // stack and runs the line after the one from the stack.
  | GoSub _ | Return -> failwith "not implemented"

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

let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

// TODO: Add empty stack of return line numbers here
let empty = { Program = []; Variables = Map.empty; Random = System.Random() }

let nim = 
  [ Some 10, Assign("M", num 20)
    Some 20, Assign("U", num 1)
    Some 30, GoSub(100)
    Some 40, Assign("U", num 2)
    Some 50, GoSub(100)
    Some 60, Goto(20) 
    Some 100, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 110, Print [ str "PLAYER "; var "U"; str ": YOU CAN TAKE BETWEEN 1 AND "; 
      Function("MIN", [num 5; var "M"]); str " MATCHES\n" ]
    Some 120, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 130, Input("P")
    Some 140, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 120)
    Some 150, Assign("M", var "M" .- var "P")
    Some 160, If(var "M" .= num 0, Goto 200)
    Some 170, Return    
    Some 200, Print [str "PLAYER "; var "U"; str " WINS!"]
    None, Run
  ]

runInputs empty nim |> ignore
