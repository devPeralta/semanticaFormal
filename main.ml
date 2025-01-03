(* Types *)
type tipo = 
  | TyInt
  | TyBool
  | TyFun of tipo * tipo
  | TyVar of int

type expr = 
  | Num of int
  | Bool of bool
  | Var of string
  | App of expr * expr  (* aplicação de função *)
  | Fun of string * expr  (* função anônima *)

type type_env = (string * tipo) list
type equacoes = (tipo * tipo) list

(* Gera variáveis de tipo frescas *)
let prox_var = ref 0
let novo_tipo () = 
  let v = !prox_var in
  prox_var := v + 1;
  TyVar v

(* Imprime tipos de forma legível *)
let rec string_of_tipo = function
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (t1, t2) -> "(" ^ string_of_tipo t1 ^ " -> " ^ string_of_tipo t2 ^ ")"
  | TyVar n -> "t" ^ string_of_int n

(* Coleta equações de tipo *)
let rec collect (env: type_env) (e: expr) : tipo * equacoes =
  match e with
  | Num _ -> (TyInt, [])
  | Bool _ -> (TyBool, [])
  | Var x ->
      (match List.assoc_opt x env with
       | Some t -> (t, [])
       | None -> failwith ("Variável não encontrada: " ^ x))
  | Fun (x, e1) ->
      let param_tipo = novo_tipo () in
      let (t1, eqs1) = collect ((x, param_tipo) :: env) e1 in
      (TyFun (param_tipo, t1), eqs1)
  | App (e1, e2) ->
      let (t1, eqs1) = collect env e1 in
      let (t2, eqs2) = collect env e2 in
      let ret_tipo = novo_tipo () in
      (ret_tipo, (t1, TyFun (t2, ret_tipo)) :: (eqs1 @ eqs2))

(* Testa se variável ocorre em tipo *)
let rec occurs (v: int) (t: tipo) : bool =
  match t with
  | TyInt | TyBool -> false
  | TyFun (t1, t2) -> occurs v t1 || occurs v t2
  | TyVar n -> v = n

(* Unificação *)
let rec unify (eqs: equacoes) : (int * tipo) list =
  match eqs with
  | [] -> []
  | (t1, t2) :: rest when t1 = t2 -> unify rest
  | (TyVar n, t) :: rest | (t, TyVar n) :: rest ->
      if occurs n t then
        failwith "Ocorre check falhou"
      else
        let subst = [(n, t)] in
        let rest' = List.map (fun (t1, t2) -> 
          (subst_tipo subst t1, subst_tipo subst t2)) rest in
        (n, t) :: unify rest'
  | (TyFun (t1, t2), TyFun (t3, t4)) :: rest ->
      unify ((t1, t3) :: (t2, t4) :: rest)
  | _ -> failwith "Tipos não unificáveis"

(* Aplica substituição em tipo *)
and subst_tipo (s: (int * tipo) list) (t: tipo) : tipo =
  match t with
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyFun (t1, t2) -> TyFun (subst_tipo s t1, subst_tipo s t2)
  | TyVar n ->
      match List.assoc_opt n s with
      | Some t' -> t'
      | None -> TyVar n

(* Função principal de inferência *)
let infer (e: expr) : tipo =
  prox_var := 0;  (* Reset contador de variáveis *)
  let (t, eqs) = collect [] e in
  let subst = unify eqs in
  let final_tipo = subst_tipo subst t in
  
  (* Imprime informações do processo *)
  print_endline "\nEquações coletadas:";
  List.iter (fun (t1, t2) ->
    Printf.printf "%s = %s\n" (string_of_tipo t1) (string_of_tipo t2)
  ) eqs;
  
  print_endline "\nSubstituições:";
  List.iter (fun (n, t) ->
    Printf.printf "t%d -> %s\n" n (string_of_tipo t)
  ) subst;
  
  print_endline "\nTipo final:";
  print_endline (string_of_tipo final_tipo);
  final_tipo