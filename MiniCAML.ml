(* TODO: Write a good set of tests for unused_vars. *)
let unused_vars_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *) (*(Let ("x", I 1, I 5), ["x"]);*)
  
  (*buggy implementation 5*) 
  (Apply ((Fn ([], Let ("x", I 1, I 5))), []),["x"]); 
  (Apply ((Fn ([], I 4)), [Primop(Plus, [Var "x";I 2])]),[]);
  (Apply ((Fn ([], Let ("x", I 1, I 5))), [I 4; I 3]),["x"]); 
  
  (*buggy implementation 1&2*) 
  (Rec ("PlusOne", Arrow([Int], Int), 
        Fn([("x", Int);("y", Int)], 
           Primop (Plus, [Var "x"; I 1]))), ["y";"PlusOne"]);
  (Rec ("PlusOne", Arrow([Int], Int), 
        Fn([], Primop (Plus, [Var "PlusOne"; I 1]))), []);
  
  (*buggy implementation 3&4*) 
  (Fn ([("y", Int);("w", Bool)], I 3),["y";"w"]); 
  (Fn ([("y", Int)], Var "y"),[]);
]

(* TODO: Implement the missing cases of unused_vars. *)
let rec unused_vars =
  function
  | Var _ | I _ | B _ -> []
  | If (e, e1, e2) -> unused_vars e @ unused_vars e1 @ unused_vars e2
  | Primop (_, args) ->
      List.fold_left (fun acc exp -> acc @ unused_vars exp) [] args
  | Let (x, e1, e2) ->
      let unused = unused_vars e1 @ unused_vars e2 in
      if List.mem x (free_variables e2) then unused
      else x :: unused 
  | Rec (x, _, e) -> 
      if List.mem x (free_variables e) then unused_vars e
      else  [x]@(unused_vars e) 
  | Fn (xs, e) -> let rec aux1 l= match l with
      |[]->[] |(name,_)::xs -> if List.mem name (free_variables e) then aux1 xs
          else [name]@(aux1 xs) in (aux1 xs)@(unused_vars e)
  | Apply (e, es) -> 
      (unused_vars e) @ (List.fold_left (fun l x -> l@(unused_vars x)) [] es)
        (*let rec aux2 l = match l with
            | [] -> [] | x::xs -> (unused_vars x)@(aux2 xs) in
         (unused_vars e) @ (aux2 es) *)

(* TODO: Write a good set of tests for subst. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let subst_tests : (((exp * name) * exp) * exp) list = [
  (* An example test case. If you have trouble writing test cases of the
     proper form, you can try copying this one and modifying it.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (*(((I 1, "x"), (* [1/x] *)
    (* let y = 2 in y + x *)
     Let ("y", I 2, Primop (Plus, [Var "y"; Var "x"]))),
   (* let y = 2 in y + 1 *)
    Let ("y", I 2, Primop (Plus, [Var "y"; I 1])));*)
  
  (*bug implementation 7*)
  (((I 5, "y"), Apply (Let ("x", I 7, Primop(Plus, [Var "x";Var "y"])),[I 4; I 3])),
   Apply (Let ("x", I 7, Primop(Plus, [Var "x";I 5])),[I 4; I 3])); 
  (*(*bug implementation 8*)
    (((I 5, "z"), Apply ((Let ("x", I 1, I 5)), [Var "z"; Var "z"])),
     Apply ((Let ("x", I 1, I 5)), [I 5; I 5]));*)
  (((I 5, "z"), Apply (ex1, [Var "z"; Var "z"])), Apply (ex1, [I 5; I 5]));
  
  (*bug implementation 1*)
  (((I 5, "z"), Rec ("y", Int, Let ("x", I 1, Var "z"))),
   Rec ("y", Int, Let ("x", I 1, I 5)));
  (*bug implementation 2*)
  (((I 5, "z"), Rec ("z", Int, Let ("x", I 1, Var "z"))),
   Rec ("z", Int, Let ("x", I 1, Var "z")));
  (*bug implementation 3*)
  (((Let ("y", I 1, Var "p"),"z"), Rec ("p", Int, Let ("x", I 1, Var "z"))),
   Rec ("w", Int, Let ("x", I 1, Let ("y", I 1, Var "p")))); 
  (*(((Let ("y", I 1, Var "p"),"z"), Rec ("p", Int, Let ("x", I 1, Var "z"))),
    Rec ("p", Int, Let ("x", I 1, Let ("y", I 1, Var "w"))));*)
  
  (*bug implementation 4*)
  (((I 5, "h"), Fn ([("y", Int);("w", Bool)],Let ("x", I 1, Var "h"))),
   Fn ([("y", Int);("w", Bool)],Let ("x", I 1, I 5)));
  (*bug implementation 5*) 
  (((I 1, "x"), Fn ([("x", Int); ("y", Int)], Primop(Plus,[Var "x";Var"y"]))),
   Fn ([("x", Int); ("y", Int)], Primop(Plus,[Var "x";Var"y"]))); 
  (*bug implementation 6*)
  (((Let ("y", I 1, Var "p"),"z"),
    Fn ([("p", Int);("w", Bool)],Let ("x", I 1, Var "z"))),
   Fn ([("p'", Int);("w", Bool)],Let ("x", I 1, Let ("y", I 1, Var "p")))); 
  (*(((Let ("y", I 1, Var "p"),"z"), 
     Fn ([("p", Int);("w", Bool)],Let ("x", I 1, Var "z"))),
    Fn ([("p", Int);("w", Bool)],Let ("x", I 1, Let ("y", I 1, Var "p'"))));*) 
]

(* TODO: Implement the missing cases of subst. *)
let rec subst ((e', x) as s) exp =
  match exp with
  | Var y ->
      if x = y then e'
      else Var y
  | I n -> I n
  | B b -> B b
  | Primop (po, args) -> Primop (po, List.map (subst s) args)
  | If (e, e1, e2) ->
      If (subst s e, subst s e1, subst s e2)
  | Let (y, e1, e2) ->
      let e1' = subst s e1 in
      if y = x then
        Let (y, e1', e2)
      else
        let (y, e2) =
          if List.mem y (free_variables e') then
            rename y e2
          else
            (y, e2)
        in
        Let (y, e1', subst s e2)

  | Rec (y, t, e) -> if y=x then Rec (y, t, e) 
      else if List.mem y (free_variables e') then let (y, e1) = rename y e in
        Rec (y, t, subst s e1)
      else Rec (y, t, subst s e)

  | Fn (xs, e) -> 
      let (names, types) = List.split xs in 
      if List.exists (fun y -> y=x) names then Fn (xs, e)
      else if List.exists (fun y -> List.mem y (free_variables e')) names then
        let (names',e') = rename_all names e in 
        Fn (List.combine names' types, subst s e') 
      else Fn (xs, subst s e)

  | Apply (e, es) -> 
      Apply (subst s e, List.map (subst s) es) 

and rename x e =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

and rename_all names exp =
  List.fold_right
    (fun name (names, exp) ->
       let (name', exp') = rename name exp in
       (name' :: names, exp'))
    names
    ([], exp)

(* Applying a list of substitutions to an expression, leftmost first *)
let subst_list subs exp =
  List.fold_left (fun exp sub -> subst sub exp) exp subs


(* TODO: Write a good set of tests for eval. *)
let eval_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec and Apply!
  *) (*(Let ("x", I 1, Primop (Plus, [Var "x"; I 5])), I 6);*)
  
  (*Buggy implementation 1*) 
  (Rec ("PlusOne", Arrow([Int], Int), Fn([("x", Int)], 
                                         Primop (Plus, [Var "PlusOne"; I 1]))), 
   Fn([("x", Int)], 
      Primop (Plus, 
              [Rec ("PlusOne", Arrow([Int], Int), 
                    Fn([("x", Int)], 
                       Primop (Plus, [Var "PlusOne"; I 1]))); I 1])));
  
  (*(Rec ("PlusOne", Arrow([Int], Int), Fn([], Primop (Plus, [I 2; I 1]))), 
    Fn([], Primop (Plus, [I 2; I 1])));*)
  (*(Rec ("PlusOne", Arrow([Int], Int), Fn([("x", Int)], Primop (Plus, 
  [Var "x"; I 1]))), Fn([("x", Int)], Primop (Plus, [Var "x"; I 1])));*)
  
  (*Buggy implementation 4*)
  (Apply (Fn ([], B true), []), B true); 
  
  (*Buggy implementation 3&6*)
  (Apply (Fn ([("x", Int); ("y", Int)],
              Primop (Plus,
                      [Primop (Times, [Var "x"; Var "x"]);
                       Primop (Times, [Var "y"; Var "y"])])), [I 3; I 4]), I 25); 
  
  (*Buggy implementation 2&5&6*) 
  (Apply 
     (Apply ( 
         Fn ([("x", Int)], 
             Fn ([("y", Int)],
                 Primop (Plus,
                         [Primop (Times, [Var "x"; Var "x"]);
                          Primop (Times, [Var "y"; Var "y"])]))), 
         [I 3]), [I 4]) , I 25);
]

(* TODO: Implement the missing cases of eval. *)
let rec eval exp =
  match exp with
  (* Values evaluate to themselves *)
  | I _ -> exp
  | B _ -> exp
  | Fn _ -> exp

  (* This evaluator is _not_ environment-based. Variables should never
     appear during evaluation since they should be substituted away when
     eliminating binding constructs, e.g. function applications and lets.
     Therefore, if we encounter a variable, we raise an error.
*)
  | Var x -> raise (Stuck (Free_variable x))

  (* Primitive operations: +, -, *, <, = *)
  | Primop (po, args) ->
      let args = List.map eval args in
      begin
        match eval_op po args with
        | None -> raise (Stuck Bad_primop_args)
        | Some v -> v
      end

  | If (e, e1, e2) ->
      begin
        match eval e with
        | B true -> eval e1
        | B false -> eval e2
        | _ -> raise (Stuck If_non_true_false)
      end

  | Let (x, e1, e2) ->
      let e1 = eval e1 in
      eval (subst (e1, x) e2)

  | Rec (f, _, e) -> eval (subst (exp, f) e)

  | Apply (e, es) -> match eval e with 
    | Fn (l,exp) -> let (names, _) = List.split l in
        if (List.length es)!=(List.length l) then raise (Stuck Arity_mismatch)
        else let subs = List.combine es names in 
          eval (subst_list subs exp)
    | _ -> raise (Stuck Apply_non_fn)

(* TODO: Write a good set of tests for infer. *)
let infer_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *) 
  (*(([("x", Int)], Var "x"), Int);*)
  
  (*buggy implementation 3*)
  (([],Fn ([("x", Int); ("y", Bool)], I 7)), Arrow([Int; Bool], Int));
  (*buggy implementation 4*)
  (([],Fn ([], Primop(Plus,[I 1;I 4]))), Arrow([], Int));
  (*buggy implementation 2*)
  (([],Fn ([("y", Int)], Fn ([("x", Int)], I 1))), 
   Arrow([Int], Arrow([Int], Int)));
  
  (*buggy implementation 1*)(*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*)
  (([],
    Rec ("fact",
         Arrow ([Int], Int),
         Fn ([("n", Int)],
             If (
               Primop (Equals, [Var "n"; I 0]), I 1,
               Primop (Times, [Var "n"; 
                               Apply (Var "fact", 
                                      [Primop (Minus, [Var "n"; I 1])])]))))),
   Arrow([Int], Int));
  
  (*buggy implementation 7*)
  (([], Apply ((Fn ([], I 7)), [])), Int);
  (*buggy implementation 6*)
  (([], Apply ((Fn ([("x", Int);("y", Int)], I 7)), [I 7; I 9])), Int);
  (*buggy implementation 5*)
  (([], Apply ((Fn ([("x", Int)], I 7)), [I 7])), Int);
]

(* TODO: Implement the missing cases of infer. *)
let rec infer ctx e =
  match e with
  | Var x ->
      begin
        try lookup x ctx
        with Not_found -> raise (TypeError (Free_variable x))
      end
  | I _ -> Int
  | B _ -> Bool

  | Primop (po, exps) ->
      let (domain, range) = primopType po in
      check ctx exps domain range

  | If (e, e1, e2) ->
      begin
        match infer ctx e with
        | Bool ->
            let t1 = infer ctx e1 in
            let t2 = infer ctx e2 in
            if t1 = t2 then t1
            else type_mismatch t1 t2
        | t -> type_mismatch Bool t
      end

  | Let (x, e1, e2) ->
      let t1 = infer ctx e1 in
      infer (extend ctx (x, t1)) e2

  | Rec (f, t, e) -> let t1 = infer (extend ctx (f,t)) e in 
      if t1 = t then t1 else type_mismatch t t1

  | Fn (xs, e) -> let (_, types) = List.split xs in 
      Arrow (types, infer (extend_list ctx xs) e)
  
  | Apply (e, es) -> match infer ctx e with 
    | Arrow(types, tp) -> check ctx es types tp 
    | itp -> raise (TypeError (Apply_non_arrow itp))

and check ctx exps tps result =
  match exps, tps with
  | [], [] -> result
  | e :: es, t :: ts ->
      let t' = infer ctx e in
      if t = t' then check ctx es ts result
      else type_mismatch t t'
  | _ -> raise (TypeError Arity_mismatch)

(* TODO: Implement type unification. *)
let rec unify (t1 : utp) (t2 : utp) : unit =
  match t1, t2 with
  (* unifying identical concrete types does nothing *)
  | UInt, UInt
  | UBool, UBool -> ()
  (* For type constructors, recursively unify the parts *)
  | UArrow (t1, t1'), UArrow (t2, t2') ->
      unify t1 t2; unify t1' t2'
  | UTVar a, _ -> unifyVar a t2
  | _, UTVar b -> unifyVar b t1
  (* All other cases are mismatched types. *)
  | _, _ -> unif_error @@ UnifMismatch (t1, t2)

(* Unify a variable with a type *)
and unifyVar a t = match a with
  | {contents = Some utp1} -> unify utp1 t
  | {contents = None} -> (match t with (*check whether t is itself UTVar b*)
      | UTVar b -> (match b with
          | {contents = Some utp2} -> unifyVar a utp2
          | {contents = None} -> if a==b then () else a := Some (UTVar b))
      | _ -> if occurs a t then unif_error @@ UnifOccursCheckFails
          else a := Some t
    )
      
      
