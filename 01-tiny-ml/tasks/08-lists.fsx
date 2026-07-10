// ----------------------------------------------------------------------------
// 08 - Add unit and create a list value
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  | ValCase of bool * Value
  // NOTE: A value that represents "empty value" and is
  // useful as the value for representing the empty list.
  | ValUnit 

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression
  | Recursive of string * Expression * Expression
  // NOTE: An expression that evaluates to a unit value.
  // This exists in F# too and it is written as '()'
  | Unit 

and VariableContext = 
  Map<string, Lazy<Value>>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
  match e with 
  | Constant n -> ValNum n
  | Binary(op, e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1, v2 with 
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | "%" -> ValNum(n1 % n2)
          | _ -> failwith "unsupported binary operator"
      | _ -> failwith "invalid argument of binary operator"
  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res.Value
      | _ -> failwith ("unbound variable: " + v)

  // NOTE: You have the following from before
  | Unary(op, e) ->
      // TODO: Implement the case for 'Unary' here!
      let v = evaluate ctx e
      match v with
      | ValNum n ->
        match op with
        | "-" -> ValNum (-n)
        | "!" -> if n = 0 then ValNum 1 else ValNum 0
        | _   -> failwith ("unsupported unary operator: " + op)
      | _ -> failwith "unary operator cannot be applied"
  // TODO: Add the correct handling of 'If' here!
  | If(cond, tb, fb) ->
    let vCond = evaluate ctx cond  
    match vCond with
    | ValNum c -> 
      if c = 1 then 
        evaluate ctx tb 
      else 
        evaluate ctx fb
    | _ -> failwith "not a condition"
    
  
  | Lambda(v, e) ->
      // TODO: Evaluate a lambda - create a closure value
      ValClosure(v, e, ctx)      

  | Application(e1, e2) ->
      // TODO: Evaluate a function application. Recursively
      // evaluate 'e1' and 'e2'; 'e1' must evaluate to a closure.
      // You can then evaluate the closure body.
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1 with
      | ValClosure(paramName, body, capturedCtx) ->
        let newCtx = Map.add paramName (lazy v2) capturedCtx
        evaluate newCtx body
      | _ -> failwith "for ValClosure only"
  
  | Let(v, e1, e2) ->
    // TODO: There are two ways to do this! A nice tricky is to 
    // treat 'let' as a syntactic sugar and transform it to the
    // 'desugared' expression and evaluating that :-)
    let value = lazy (evaluate ctx e1)
    let newCtx = Map.add v value ctx
    evaluate newCtx e2

  | Tuple(e1, e2) ->
      // TODO: Construct a tuple value here!
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      ValTuple(v1, v2)
  | TupleGet(b, e) ->
      // TODO: Access #1 or #2 element of a tuple value.
      // (If the argument is not a tuple, this fails.)
      let t = evaluate ctx e
      match t with
      | ValTuple(v1, v2) ->
        match b with
        | true -> v1
        | false -> v2
      | _ -> failwith "not a tuple" 
  | Match(e, v, e1, e2) ->
      // TODO: Implement pattern matching. Note you need to
      // assign the right value to the variable of name 'v'!
      let ve = evaluate ctx e
      match ve with
      | ValCase(b, value) ->
      let lazyValue = lazy value
      let newCtx = Map.add v lazyValue ctx
      
      if b then
        evaluate newCtx e1
      else
        evaluate newCtx e2
      | _ -> failwith "not a case"

  | Case(b, e) ->
      // TODO: Create a union value.
      let v = evaluate ctx e
      ValCase(b, v)
      
  | Recursive(v, e1, e2) ->
      // TODO: Implement recursion for 'let rec v = e1 in e2'.
      // (In reality, this will only work if 'e1' is a function
      // but the case can be implemented without assuming that).
      let rec newCtx = 
          let lazyVal = lazy (evaluate newCtx e1)
          Map.add v lazyVal ctx
      evaluate newCtx e2
  
  // NOTE: This is so uninteresting I did this for you :-)
  | Unit -> ValUnit


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Ultimate functional programming - lists and List.map!
// We represent lists as cons cells using tuples, so [1,2,3]
//
// = Case(true, Tuple(Constant(1), Case(true, Tuple(Constant(2), 
//     Case(true, Tuple(Constant(3), Case(false, Unit) ))))))

// Helper function to construct lists, so that we 
// do not need to write them by hand!
let rec makeListExpr l = 
  match l with
  | x::xs -> Case(true, Tuple(x, makeListExpr xs))
  | [] -> Case(false, Unit)

let el = makeListExpr [ for i in 1 .. 5 -> Constant i ]

// List.map function in TinyML:
//
//   let rec map = (fun f -> fun l -> 
//     match l with 
//     | Case1 t -> Case1(f x#1, (map f) x#2) 
//     | Case2(Unit) -> Case2(Unit))
//   in map (fun y -> y * 10) l
//
let em = 
  Recursive("map",
    Lambda("f", Lambda("l", 
      Match(
        Variable("l"), "x",
        Case(true, Tuple(
          Application(Variable "f", TupleGet(true, Variable "x")),
          Application(Application(Variable "map", Variable "f"), 
            TupleGet(false, Variable "x"))
        )),
        Case(false, Unit)
      )
    )),
    Application(Application(Variable "map", 
      Lambda("y", Binary("*", Variable "y", Constant 10))), el)
  )
evaluate Map.empty em

// TODO: Can you implement 'List.filter' in TinyML too??
// The somewhat silly example removes 3 from the list.
// Add '%' binary operator and you can remove odd/even numbers!
//
//   let rec filter = (fun f -> fun l -> 
//     match l with 
//     | Case1 t -> 
//          if f x#1 then Case1(x#1, (filter f) x#2) 
//          else (filter f) x#2
//     | Case2(Unit) -> Case2(Unit))
//   in filter (fun y -> y + (-2)) l
//

let ef = 
  Recursive("filter",
    Lambda("f", Lambda("l", 
      Match(
        Variable("l"), "x", 
        If(
          Application(Variable "f", TupleGet(true, Variable "x")),
          
          // True branch, filter out odd numbers
          Application(Application(Variable "filter", Variable "f"), TupleGet(false, Variable "x")),
          
          // False branch, keep even numbers
          Case(true, Tuple(
            TupleGet(true, Variable "x"),
            Application(Application(Variable "filter", Variable "f"), TupleGet(false, Variable "x"))
          ))
        ),
        Case(false, Unit)
      )
    )),
    Application(
      Application(Variable "filter", 
        Lambda("y", Binary("%", Variable "y", Constant 2))
      ), 
      el
    )
  )

evaluate Map.empty ef
