// ----------------------------------------------------------------------------
// 07 - Add support for recursion
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  | ValCase of bool * Value

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
  // NOTE: A recursive definition. You can think of 
  // 'Let(v, e1, e2)' as 'let rec v = e1 in e2'. 
  | Recursive of string * Expression * Expression

and VariableContext = 
  // NOTE: For recursive calls, we need to add the function
  // being defined to the variable context when defining it.
  // This can be done using 'let rec', but we need to store
  // the variables as lazy values.
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
          | _ -> failwith "unsupported binary operator"
      | _ -> failwith "invalid argument of binary operator"
  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res ->
          // NOTE: As 'res' is now 'Lazy<Value>' we need to get its value here.
          res.Value
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

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Recursion and conditionals - implementing factorial!
//   let rec factorial = fun x -> 
//     if x then 1 else x*(factorial (-1 + x))
//   in factorial 5
let er = 
  Recursive("factorial", 
    Lambda("x", If(
      Variable("x"),
      Constant(1),
      Binary(
        "*", Variable("x"), 
        Application(Variable("factorial"), 
          Binary("+", Constant(-1), Variable("x")))
      )
    )),  
    Application(Variable "factorial", Constant 5)
  )
evaluate Map.empty er
