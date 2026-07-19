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
    | Recursive of string * Expression * Expression
    | Unit
    // NOTE: To keep things simpler, we add special expressions
    // for list construction and pattern matching on lists.
    | ListCase of bool * Expression
    | ListMatch of Expression * string * Expression * Expression

type Type =
    | TyVariable of string
    | TyBool
    | TyNumber
    | TyList of Type
    | TyFunction of Type * Type
    | TyTuple of Type * Type
    | TyUnion of Type * Type
    | TyUnit

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty =
    match ty with
    | TyVariable var -> var = vcheck
    | TyBool -> false
    | TyNumber -> false
    | TyList t -> occursCheck vcheck t // check the inner part
    | TyFunction(t1, t2)
    | TyTuple(t1, t2)
    | TyUnion(t1, t2) -> occursCheck vcheck t1 || occursCheck vcheck t2
    | TyUnit -> false

let rec substType (subst: Map<_, _>) t1 =
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
    match cs with
    | [] -> []
    | (TyNumber, TyNumber) :: cs -> solve cs
    | (TyBool, TyBool) :: cs -> solve cs
    | (TyList t1, TyList t2) :: cs -> solve ((t1, t2) :: cs)
    | (TyVariable v, ty) :: cs
    | (ty, TyVariable v) :: cs ->
        if ty = TyVariable v then
          solve cs
        elif occursCheck v ty then
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

    fun () ->
        n <- n + 1
        TyVariable(sprintf "_a%d" n)

let rec generate (ctx: TypingContext) e =
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
        // As with tuples, we know the type of 'e' is some union,
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
        // This is easier than evaluation. We need a new type variable
        // for the type of the thing we are defining (variable 'v') and add
        // it to the context when checking both 'e1' and 'e2'.
        let tv = newTyVariable ()
        let newCtx = Map.add v tv ctx

        let t1, cs1 = generate newCtx e1
        let t2, cs2 = generate newCtx e2
        t2, (tv, t1) :: (cs1 @ cs2)

    | ListMatch(e, v, e1, e2) ->
        // TODO: Type of 'e' ('tylist') needs to be a list of elements ('tyel').
        // In 'e1', the type of the variable 'v' is then a tuple 'tyel * tylist'.
        // In 'e2', the type of the variable 'v' is just 'unit'.
        // To express this, you will need a new type variable for 'tyel'.
        let te, cs = generate ctx e
        let tel = newTyVariable ()

        let t1, cs1 = generate (Map.add v (TyTuple(tel, TyList tel)) ctx) e1
        let t2, cs2 = generate (Map.add v TyUnit ctx) e2
        t1, (te, TyList tel) :: (t1, t2) :: (cs @ cs1 @ cs2)

    | ListCase(true, Tuple(ehd, etl)) ->
        // TODO: If type of 'ehd' is 'tyel' and type of 'etl' is 'tylist'
        // then we need a constraint 'tylist = list<tyel>'.
        let thd, cs1 = generate ctx ehd
        let ttl, cs2 = generate ctx etl
        TyList thd, (ttl, TyList thd) :: (cs1 @ cs2)

    | ListCase(false, Unit) ->
        // TODO: The type of '[]' is a list of some type (needs a type variable)
        let t = newTyVariable ()
        TyList t, []

    | ListCase _ ->
        // TODO: For simplicity, we here restrict the syntax of list constructs.
        // In general, this is not needed, but it makes the task easier...
        failwith "unsupported list syntax"

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e =
    let typ, constraints = generate Map.empty e
    let subst = solve constraints
    let typ = substType (Map.ofList subst) typ
    typ

// NOTE: The following is modified from task 7 to use
// ListCase and ListMatch instead of normal Case and Match.
// It should all type check as expected now!

let rec makeListExpr l =
    match l with
    | x :: xs -> ListCase(true, Tuple(x, makeListExpr xs))
    | [] -> ListCase(false, Unit)

makeListExpr [ for i in 1..5 -> Constant i ] |> infer

Recursive(
    "map",
    Lambda(
        "f",
        Lambda(
            "l",
            ListMatch(
                Variable("l"),
                "x",
                ListCase(
                    true,
                    Tuple(
                        Application(Variable "f", TupleGet(true, Variable "x")),
                        Application(Application(Variable "map", Variable "f"), TupleGet(false, Variable "x"))
                    )
                ),
                ListCase(false, Unit)
            )
        )
    ),
    Variable("map")
)
|> infer
