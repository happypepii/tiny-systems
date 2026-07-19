// ----------------------------------------------------------------------------
// Adding simple data types
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: Added two types of expression for working with tuples
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  // NOTE: Added type for tuples
  | TyTuple of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  // TODO: Add case for 'TyTuple' (same as 'TyFunction')
  match ty with 
  | TyVariable var -> var = vcheck
  | TyBool -> false
  | TyNumber -> false
  | TyList t -> occursCheck vcheck t // check the inner part
  | TyFunction (t1, t2) -> 
    occursCheck vcheck t1 || occursCheck vcheck t2
  failwith "not implemented"

let rec substType (subst:Map<_, _>) t1 = 
  // TODO: Add case for 'TyFunction' (need to substitute in both nested types)
  match t1 with 
  | TyVariable var -> 
    if Map.containsKey var subst then
      subst.[var]
    else
      TyVariable var
  | TyBool -> TyBool // do nothing
  | TyNumber -> TyNumber // do nothing
  | TyList t -> TyList(substType subst t) // check the inner part
  | TyFunction (t1, t2) -> TyFunction(substType subst t1, substType subst t2)
  failwith "not implemented"

let substConstrs subst cs = 
  cs |> List.map(fun (l, r) -> substType subst l, substType subst r) 

 
let rec solve cs =
  // TODO: Add case for 'TyTuple' (same as 'TyFunction')
  match cs with 
  | [] -> []
  | (TyNumber, TyNumber)::cs -> solve cs
  | (TyBool, TyBool)::cs -> solve cs
  | (TyList t1, TyList t2)::cs -> solve ((t1, t2)::cs)
  | (TyVariable v, ty)::cs | (ty, TyVariable v)::cs ->
    if occursCheck v ty then failwith "occurs check failed"
    elif ty = TyVariable v then solve cs
    else
      let cs = substConstrs (Map.ofList [v, ty]) cs
      let subst = solve cs
      let ty = substType (Map.ofList subst) ty
      (v, ty)::subst
  | (TyFunction(ta1, tb1), TyFunction(ta2, tb2))::cs ->
    solve ((ta1, ta2) :: (tb1, tb2) :: cs)
  | _ -> failwith "Cannot be solved"
  failwith "not implemented"


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable = 
  let mutable n = 0
  fun () -> n <- n + 1; TyVariable(sprintf "_a%d" n)

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ -> 
      // NOTE: If the expression is a constant number, we return
      // its type (number) and generate no further constraints.
      TyNumber, []

  | Binary("+", e1, e2) ->
      // NOTE: Recursively process sub-expressions, collect all the 
      // constraints and ensure the types of 'e1' and 'e2' are 'TyNumber'
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

  | Binary("=", e1, e2) ->
      let t1, v1 = generate ctx e1
      let t2, v2 = generate ctx e2
      TyBool, v1 @ v2 @ [ t1, t2 ] // can be anything as long as they have the same type

  | Binary("*", e1, e2) ->
      // NOTE: Recursively process sub-expressions, collect all the 
      // constraints and ensure the types of 'e1' and 'e2' are 'TyNumber'
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

  | Binary(op, _, _) ->
      failwithf "Binary operator '%s' not supported." op

  | Variable v -> 
      let t = ctx.[v]
      t, []

  | If(econd, etrue, efalse) ->
      let t1, ec  = generate ctx econd
      let t2, et = generate ctx etrue
      let t3, ef = generate ctx efalse
      t2, ec @ et @ ef @ [ t1, TyBool; t2, t3 ] 

  | Let(v, e1, e2) ->
      let t1, ev1 = generate ctx e1
      let newCtx = Map.add v t1 ctx
      let t2, ev2 = generate newCtx e2
      t2, ev1 @ ev2
  
  | Lambda(v, e) ->
      let targ = newTyVariable()
      let newCtx = Map.add v targ ctx
      let tbody, cs = generate newCtx e
      TyFunction(targ, tbody), cs

  | Application(e1, e2) -> 
    let tfun, cs1 = generate ctx e1
    let targ, cs2 = generate ctx e2
    let tres = newTyVariable()
    tres,
      cs1 @ cs2 @
      [
        tfun, TyFunction(targ, tres)
      ]
  
  | Tuple(e1, e2) ->
      // TODO: Easy. The returned type is composed of the types of 'e1' and 'e2'.
      failwith "not implemented"

  | TupleGet(b, e) ->
      // TODO: Trickier. The type of 'e' is some tuple, but we do not know what.
      // We need to generate two new type variables and a constraint.
      failwith "not implemented"

  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// Basic tuple examples:
// * (2 = 21, 123)
// * (2 = 21, 123)#1
// * (2 = 21, 123)#2
let etup = Tuple(Binary("=", Constant(2), Constant(21)), Constant(123))
etup |> infer
TupleGet(true, etup) |> infer
TupleGet(false, etup) |> infer

// Interesting case with a nested tuple ('a * ('b * 'c) -> 'a * 'b)
// * fun x -> x#1, x#2#1
Lambda("x", Tuple(TupleGet(true, Variable "x"), 
  TupleGet(true, TupleGet(false, Variable "x"))))
|> infer

// Does not type check - 'int' is not a tuple!
// * (1+2)#1
TupleGet(true, Binary("+", Constant 1, Constant 2)) |> infer


// Combining functions and tuples ('b -> (('b -> 'a) -> ('b * 'a)))
// * fun x f -> (x, f x)   
Lambda("x", Lambda("f", 
  Tuple(Variable "x", 
    Application(Variable "f", Variable "x"))))
|> infer
