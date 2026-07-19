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
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression
  // NOTE: Added two types of expression for working with unions
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  | TyTuple of Type * Type
  // NOTE: Added type for tuples
  | TyUnion of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  // TODO: Add case for 'TyUnion' (same as 'TyFunction')
  match ty with 
  | TyVariable var -> var = vcheck
  | TyBool -> false
  | TyNumber -> false
  | TyList t -> occursCheck vcheck t // check the inner part
  | TyFunction (t1, t2) | TyTuple (t1, t2) -> 
    occursCheck vcheck t1 || occursCheck vcheck t2

  failwith "not implemented"

let rec substType (subst:Map<_, _>) t1 = 
  // TODO: Add case for 'TyUnion' (same as 'TyFunction')
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
  | TyTuple (t1, t2) -> TyTuple(substType subst t1, substType subst t2)

  failwith "not implemented"

let substConstrs subst cs = 
  cs |> List.map(fun (l, r) -> substType subst l, substType subst r) 
 
let rec solve constraints =
  // TODO: Add case for 'TyUnion' (same as 'TyFunction')
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
  | (TyTuple(ta1, tb1), TyTuple(ta2, tb2))::cs ->
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
      let t1, cs1 = generate ctx e1
      let t2, cs2 = generate ctx e2
      TyNumber, cs1 @ cs2 @ [ t1, TyNumber; t2, TyNumber ]

  | Binary("=", e1, e2) ->
      let t1, cs1 = generate ctx e1
      let t2, cs2 = generate ctx e2
      TyBool, cs1 @ cs2 @ [ t1, t2 ] // can be anything as long as they have the same type

  | Binary("*", e1, e2) ->
      // NOTE: Recursively process sub-expressions, collect all the 
      // constraints and ensure the types of 'e1' and 'e2' are 'TyNumber'
      let t1, cs1 = generate ctx e1
      let t2, cs2 = generate ctx e2
      TyNumber, cs1 @ cs2 @ [ t1, TyNumber; t2, TyNumber ]

  | Binary(op, _, _) ->
      failwithf "Binary operator '%s' not supported." op

  | Variable v -> 
      let t = ctx.[v]
      t, []

  | If(econd, etrue, efalse) ->
      let t1, cs1  = generate ctx econd
      let t2, cs2 = generate ctx etrue
      let t3, cs3 = generate ctx efalse
      t2, cs1 @ cs2 @ cs3 @ [ t1, TyBool; t2, t3 ] 

  | Let(v, e1, e2) ->
      let t1, cs1 = generate ctx e1
      let newCtx = Map.add v t1 ctx
      let t2, cs2 = generate newCtx e2
      t2, cs1 @ cs2
  
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
      let te1, cs1 = generate ctx e1
      let te2, cs2 = generate ctx e2
      TyTuple(te1, te2), cs1 @ cs2

  | TupleGet(b, e) ->
      // We need to generate two new type variables and a constraint.
      // te = TyTuple(ta, tb)
      let te, cs = generate ctx e
      let ta = newTyVariable()
      let tb = newTyVariable()
      match b with
      | true -> // get first element and tyoe
        ta, (te, TyTuple(ta, tb)) :: cs
      | false -> // get snd element and tyoe
        tb, (te, TyTuple(ta, tb)) :: cs

  | Match(e, v, e1, e2) ->
      // TODO: As with tuples, we know the type of 'e' is some union,
      // but we do not know what. We need new type variables. When 
      // checking 'e1' and 'e2', add variable 'v' to the context!
      // Also note that the return types of 'e1' and 'e2' have to match.
      failwith "not implemented"

  | Case(b, e) ->
      // TODO: Here, we know the type of 'e' is the type of one of 
      // the cases, but we still need a new type variable for the other.
      failwith "not implemented"
  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// Both cases are constrained because 'if' returns either one or the other
// * fun x -> if x = 0 then Case1(fun x -> x) else Case2(42)
Lambda("x", 
  If(Binary("=", Variable("x"), Constant(0)),
    Case(true, Lambda("x", Variable("x"))),
    Case(false, Constant(42))
  ))
|> infer

// No constraints to fix the second case type (case<number, 'a> -> number)
// * fun x -> match x with Case1 v -> v + 1 | Case2 _ -> 0 
Lambda("x", Match(Variable("x"), "v", 
  Binary("+", Variable("v"), Constant(1)),
  Constant(0)))
|> infer