module F = Format
module M = Map.Make(String)

let rec interp_expr (e: Ast.expr) (g: FStore.t) (s: Store.t) : Value.t = 
 (* Implement this function *)
    match e with
    | Num n -> NumV n
    | Add (e1, e2) ->
        let v1 = interp_expr e1 g s in
        let v2 = interp_expr e2 g s in
        (match (v1,v2) with
        (NumV n1, NumV n2) -> NumV(n1+n2))        

    | Sub (e1, e2) ->
        let v1 = interp_expr e1 g s in
        let v2 = interp_expr e2 g s in
        (match (v1,v2) with
        (NumV n1, NumV n2) -> NumV(n1-n2))

    | LetIn (x, e1, e2) -> 
        interp_expr e2 g (M.add x (interp_expr e1 g s) s)

    | Id i -> 
        if (M.mem i s) then M.find i s
        else failwith ("Free identifier: "^i)

    | Call (f, e2) -> 
        if (M.mem f g) then let ff = M.find f g in
        match ff with
        (f0, f1) ->
            let rec jg ff0 ee cnt len ss = 
            if cnt<len then jg ff0 ee (cnt+1) len (M.add (List.nth ff0 cnt) (interp_expr (List.nth ee cnt) g s) ss)
        else ss
        in
            if List.length f0 = List.length e2 then interp_expr f1 g (jg f0 e2 0 (List.length e2) s)
            else failwith ("The number of arguments of x mismatched: Required: "^string_of_int (List.length f0)^", Actual: "^string_of_int (List.length e2))
        else failwith ("Undefined function: "^f)
        

    (*Map.make 정보 - https://v2.ocaml.org/api/Map.Make.html *)
    (*add, update, exists, find 참고.*)

let interp_fundef (d: Ast.fundef) (g: FStore.t) : FStore.t = 
 (* Implement this function *)
  match d with
  | FunDef (x, pl, e) -> FStore.add x (pl, e) g

let interp (p: Ast.prog) : Value.t = 
 (* Implement this function *)
    let g = M.empty in
    let s = M.empty in
    match p with
    | Prog (dl, e) ->
        let rec jaegui dll cnt len gg =
        if cnt<len then jaegui dll (cnt+1) len (interp_fundef (List.nth dll cnt) gg)
        else gg
        in
        interp_expr e (jaegui dl 0 (List.length dl) g) s