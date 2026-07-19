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
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression
  // NOTE: Added the unit value and recursive definition
  | Recursive of string * Expression * Expression
  | Unit 

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  | TyTuple of Type * Type
  | TyUnion of Type * Type
  // NOTE: We need another primitive type for units
  | TyUnit

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  // TODO: Add case for 'TyUnit' (same as 'TyNumber' or 'TyBool')
    match ty with
    | TyVariable var -> var = vcheck
    | TyBool -> false
    | TyNumber -> false
    | TyList t -> occursCheck vcheck t // check the inner part
    | TyFunction(t1, t2)
    | TyTuple(t1, t2)
    | TyUnion(t1, t2) -> occursCheck vcheck t1 || occursCheck vcheck t2
    | TyUnit -> false

let rec substType (subst:Map<_, _>) t1 = 
  // TODO: Add case for 'TyUnit' (same as 'TyNumber' or 'TyBool')
    match t1 with
    | TyVariable var ->
        if Map.containsKey var subst then
            subst.[var]
        else
            TyVariable var
    | TyBool -> TyBool // do nothing
    | TyNumber -> TyNumber // do nothing
    | TyList t -> TyList(substType subst t) // check the inner part
    | TyFunction(t1, t2) -> TyFunction(substType subst t1, substType subst t2)
    | TyTuple(t1, t2) -> TyTuple(substType subst t1, substType subst t2)
    | TyUnion(t1, t2) -> TyUnion(substType subst t1, substType subst t2)
    | TyUnit -> TyUnit 

let substConstrs subst cs = 
  cs |> List.map (fun (l, r) -> substType subst l, substType subst r)
 
let rec solve cs =
  // TODO: Add case for 'TyUnit' (same as 'TyNumber' or 'TyBool')
    match cs with
    | [] -> []
    | (TyNumber, TyNumber) :: cs -> solve cs
    | (TyBool, TyBool) :: cs -> solve cs
    | (TyList t1, TyList t2) :: cs -> solve ((t1, t2) :: cs)
    | (TyVariable v, ty) :: cs
    | (ty, TyVariable v) :: cs ->
        if occursCheck v ty then
            failwith "occurs check failed"
        elif ty = TyVariable v then
            solve cs
        else
            let cs = substConstrs (Map.ofList [ v, ty ]) cs
            let subst = solve cs
            let ty = substType (Map.ofList subst) ty
            (v, ty) :: subst
    | (TyFunction(ta1, tb1), TyFunction(ta2, tb2)) :: cs
    | (TyTuple(ta1, tb1), TyTuple(ta2, tb2)) :: cs
    | (TyUnion(ta1, tb1), TyUnion(ta2, tb2)) :: cs -> solve ((ta1, ta2) :: (tb1, tb2) :: cs)
    | (TyUnit, TyUnit) :: cs -> solve cs
    | _ -> failwith "Cannot be solved"




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

    | Binary(op, _, _) -> failwithf "Binary operator '%s' not supported." op

    | Variable v ->
        let t = ctx.[v]
        t, []

    | If(econd, etrue, efalse) ->
        let t1, cs1 = generate ctx econd
        let t2, cs2 = generate ctx etrue
        let t3, cs3 = generate ctx efalse
        t2, cs1 @ cs2 @ cs3 @ [ t1, TyBool; t2, t3 ]

    | Let(v, e1, e2) ->
        let t1, cs1 = generate ctx e1
        let newCtx = Map.add v t1 ctx
        let t2, cs2 = generate newCtx e2
        t2, cs1 @ cs2

    | Lambda(v, e) ->
        let targ = newTyVariable ()
        let newCtx = Map.add v targ ctx
        let tbody, cs = generate newCtx e
        TyFunction(targ, tbody), cs

    | Application(e1, e2) ->
        let tfun, cs1 = generate ctx e1
        let targ, cs2 = generate ctx e2
        let tres = newTyVariable ()
        tres, cs1 @ cs2 @ [ tfun, TyFunction(targ, tres) ]

    | Tuple(e1, e2) ->
        let te1, cs1 = generate ctx e1
        let te2, cs2 = generate ctx e2
        TyTuple(te1, te2), cs1 @ cs2

    | TupleGet(b, e) ->
        // We need to generate two new type variables and a constraint.
        // te = TyTuple(ta, tb)
        let te, cs = generate ctx e
        let ta = newTyVariable ()
        let tb = newTyVariable ()

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
        let te, cs = generate ctx e
        let ta = newTyVariable ()
        let tb = newTyVariable ()

        let t1, cs1 = generate (Map.add v ta ctx) e1
        let t2, cs2 = generate (Map.add v tb ctx) e2

        t1, (te, TyUnion(ta, tb)) :: (t1, t2) :: (cs @ cs1 @ cs2)

    | Case(b, e) ->
        // Here, we know the type of 'e' is the type of one of
        // the cases, but we still need a new type variable for the other.
        let te, cs = generate ctx e
        let t = newTyVariable ()

        match b with
        | true -> TyUnion(te, t), cs
        | false -> TyUnion(t, te), cs

    | Unit -> 
      // NOTE: This is so easy I wrote it for you :-)
      TyUnit, []

    | Recursive(v, e1, e2) ->
        // TODO: This is easier than evaluation. We need a new type variable
        // for the type of the thing we are defining (variable 'v') and add
        // it to the context when checking both 'e1' and 'e2'.
        let tv = newTyVariable()
        let newCtx = Map.add v tv ctx

        let t1, cs1 = generate newCtx e1
        let t2, cs2 = generate newCtx e2
        t2, (tv, t1) :: (cs1 @ cs2)
  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// Helper to generate list 1 .. 5 from TinyML tasks
let rec makeListExpr l = 
  match l with
  | x::xs -> Case(true, Tuple(x, makeListExpr xs))
  | [] -> Case(false, Unit)

// We can type check this, but the type is horrible!
makeListExpr [ for i in 1 .. 5 -> Constant i ]
|> infer 

// Code for the List.map function from TinyML task. This fails to check.
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
  Variable("map"))
|> infer 
