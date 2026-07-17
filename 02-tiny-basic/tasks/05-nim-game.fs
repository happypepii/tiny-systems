// ----------------------------------------------------------------------------
// 05 - A few more functions and operators
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
  // NOTE: Input("X") reads a number from console and assigns it to X;
  // Stop terminates the program; I also modified Print to take a list of
  // expressions instead of just one (which is what C64 supports too).
  | Print of Expression list
  | Input of string 
  | Stop

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Random : System.Random }

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

let empty = { Program = []; Variables = Map.empty; Random = System.Random() }

// NOTE: A simple game you should be able to run now! :-)
let nim = 
  [ Some 10, Assign("M", num 20)
    Some 20, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 30, Print [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 50, Input("P")
    Some 60, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 40)
    Some 70, Assign("M", var "M" .- var "P")
    Some 80, If(var "M" .= num 0, Goto 200)
    Some 90, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 100, Print [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 120, Input("P")
    Some 130, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 110)
    Some 140, Assign("M", var "M" .- var "P")
    Some 150, If(var "M" .= num 0, Goto 220)
    Some 160, Goto 20
    Some 200, Print [str "PLAYER 1 WINS!"]
    Some 210, Stop
    Some 220, Print [str "PLAYER 2 WINS!"]
    Some 230, Stop
    None, Run
  ]

runInputs empty nim |> ignore
