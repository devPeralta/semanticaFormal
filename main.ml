(* Linguagem de tipos *)

type tipo = 
    TyInt 
  | TyBool
  | TyFn   of tipo * tipo
  | TyList of tipo          
  | TyVar  of int  
  | TyMaybe of tipo

            
(* para impressao legível de tipos  *)

let rec tipo_str (tp:tipo) : string =
  match tp with
    TyInt           -> "int"
  |  TyBool          -> "bool"       
  |  TyFn   (t1,t2)  -> "("  ^ (tipo_str t1) ^
                        "->" ^ (tipo_str t2) ^ ")"
  |  TyVar  x        -> (string_of_int x)
  |  TyList t1       -> "(" ^ (tipo_str t1) ^ " list)"
  |  TyMaybe t1      -> "(" ^ (tipo_str t1) ^ " maybe)"


type op = Sum | Sub | Mult | Eq | Gt | Lt | Geq | Leq | And | Or

type ident = string

(* Expressões de L1  - sem anotação de tipo  *)
                                                
type expr  = 
    Num of int  
  | Var of ident 
  | True
  | False
  | Binop of op * expr * expr
  | If of expr * expr * expr
  | Fn of ident * expr                    
  | App of expr * expr
  | Let of ident * expr * expr           
  | LetRec of ident * ident * expr * expr 
  | Nil                                  
  | Cons of expr * expr
  | Head of expr
  | Tail of expr
  | IsEmpty of expr
  | MatchList of expr * expr * ident * ident * expr
  | Nothing 
  | Just of expr 
  | MatchMaybe of expr * expr * ident * expr 

(* para impressão legível de expressões *)
     
let rec expr_str (e:expr) : string  =
  match e with
    Var x -> x            
  | Num n -> string_of_int n;
  | True  -> "true"
  | False -> "false"
  | Binop (o,e1,e2) ->  
      let s = (match o with
            Sum  -> "+"
          | Sub  -> "-"
          | Mult -> "*"
          | Leq  -> "<="
          | Geq  -> ">="
          | Eq   -> "="
          | Lt   -> "<"
          | Gt   -> ">"
          | And -> "&&"
          | Or  -> "||"
          ) in
      "( " ^ (expr_str e1) ^ " " ^ s ^ " " ^ (expr_str e2) ^ " )"
  | If (e1,e2,e3) -> "(if " ^ (expr_str e1) ^ " then "
                     ^ (expr_str e2) ^ " else "
                     ^ (expr_str e3) ^ " )" 
  | Fn (x,e1) -> "(fn " ^ x ^ " => " ^ (expr_str e1) ^ " )"
  | App (e1,e2) -> "(" ^ (expr_str e1) ^ " " ^ (expr_str e2) ^ ")"
  | Let (x,e1,e2) -> "(let " ^ x ^ "=" ^ (expr_str e1) ^ "\nin " ^ (expr_str e2) ^ " )"
  | LetRec (f,x,e1,e2) -> "(let rec" ^ f ^ "= fn " ^ x ^ " => " ^ (expr_str e1) ^ "\nin " ^ (expr_str e2) ^ " )"
  | Nil -> "[]"
  | Cons (e1,e2) -> (expr_str e1) ^ "::" ^ (expr_str e2)
  | Head e1 -> "hd " ^ (expr_str e1)
  | Tail e1 -> "tl " ^ (expr_str e1)
  | IsEmpty e1 -> "isEmpty " ^ (expr_str e1)
  | MatchList(e1, e2, x, xs, e3) -> 
    "match " ^ (expr_str e1) ^ " with\n" ^
    "| nil -> " ^ (expr_str e2) ^ "\n" ^
    "| " ^ x ^ "::" ^ xs ^ " -> " ^ (expr_str e3)
  | Nothing -> "nothing"
  | Just e1 -> "just " ^ (expr_str e1)
  | MatchMaybe(e1, e2, x, e3) ->
      "match " ^ (expr_str e1) ^ " with\n" ^
      "| nothing -> " ^ (expr_str e2) ^ "\n" ^
      "| just " ^ x ^ " -> " ^ (expr_str e3)


(* ambientes *)
            
type tyenv = (ident * tipo) list 
    
let empty_amb : tyenv = []
    
let rec lookup g x : tipo option = 
  match g with
    [] -> None
  | (y,t) :: tl -> if (y=x) then Some t else lookup tl x
  
let update (g: tyenv) (x:ident) (t:tipo) : tyenv = 
  (x,t) :: g

  
               
(* restrições de tipo  *)
  
type constraints = (tipo * tipo) list 

(* 
   a lista 
       [ (t1,t2) ; (u1,u2) ] 
   representa o conjunto de restrições 
       { t1=t2, u1=u2 } 
 *)
                 

(* imprime restrições *)

let rec print_constr (c:constraints) =
  match c with
    []       -> print_string "\n";
  | (t1,t2)::c' -> 
      print_string (tipo_str t1);
      print_string " = ";
      print_string (tipo_str t2);
      print_string "\n";
      print_constr c'


(* código para geração de novas variáveis de tipo *)
                 
let lastvar : int ref = ref 0

let newvar (u:unit) : int =
  let x:int = !lastvar
  in lastvar := (x+1) ; x


(*  coleta de restrições *)

exception CollectFail of string
       
let rec collect (g:tyenv) (e:expr) : (constraints * tipo)  =

  match e with

  | Var x ->
      (match lookup g x with
         None    -> raise (CollectFail x)
       | Some tp -> ([],tp))                    

  | Num n -> ([],TyInt)

  | True  -> ([],TyBool)

  | False -> ([],TyBool)

  | Binop (o,e1,e2) ->  
    if List.mem o [Sum;Sub;Mult]
      then
        let (c1,tp1) = collect g e1 in
        let (c2,tp2) = collect g e2 in
        (c1@c2@[(tp1,TyInt);(tp2,TyInt)] , TyInt)
      else if List.mem o [And;Or]
      then
        let (c1,tp1) = collect g e1 in
        let (c2,tp2) = collect g e2 in
        (c1@c2@[(tp1,TyBool);(tp2,TyBool)] , TyBool)
      else   
        let (c1,tp1) = collect g e1 in
        let (c2,tp2) = collect g e2 in
        (c1@c2@[(tp1,TyInt);(tp2,TyInt)] , TyBool)

  | If (e1,e2,e3) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      let (c3,tp3) = collect g e3 in         
      (c1@c2@c3@[(tp1,TyBool);(tp2,tp3)], tp2)

  | Fn (x,e1) ->
      let tA       = newvar()        in
      let g'       = (x,TyVar tA)::g in
      let (c1,tp1) = collect g' e1   in
      (c1, TyFn(TyVar tA,tp1))
         
  | App (e1,e2) -> 
      let (c1,tp1) = collect  g e1  in
      let (c2,tp2) = collect  g e2  in
      let tX       = newvar()       in 
      (c1@c2@[(tp1,TyFn(tp2, TyVar tX))]
      , TyVar tX) 
         
  | Let (x,e1,e2) ->
      let (c1,tp1) = collect  g e1   in
      let tX       = newvar()        in 
      let g'       = (x,TyVar tX)::g       in
      let (c2,tp2) = collect  g' e2  in
      (c1@c2@[(TyVar tX,tp1)], tp2)

  | LetRec (f,x,e1,e2) ->
      let tX = newvar() in
      let tY = newvar() in
      let g2 = (f,TyVar tX)::g in
      let g1 = (x,TyVar tY)::g2               in
      let (c1,tp1) = collect g1 e1            in
      let (c2,tp2) = collect g2 e2            in
      (c1@c2@[(TyVar tX,TyFn(TyVar tY,tp1))],   tp2)
     
  | Nil ->
      let tA = newvar() in
      ([], TyList (TyVar tA))
     
  | Cons (e1,e2) ->
      let (c1,tp1) = collect g e1 in
      let (c2,tp2) = collect g e2 in
      (c1@c2@[(tp2,TyList tp1)], tp2)
             
  | Head e1 ->
      let (c1,tp1) = collect g e1 in
      let tA = newvar() in
      (c1@[(tp1,TyList (TyVar tA))], TyVar tA)
         
  | Tail e1 ->
      let (c1,tp1) = collect g e1 in
      let tA = newvar() in
      (c1@[(tp1,TyList (TyVar tA))], tp1)
  
  | IsEmpty e1 ->
      let (c1,tp1) = collect g e1 in
      let tA = newvar() in
      (c1@[(tp1,TyList (TyVar tA))], TyBool)

  | MatchList(e1, e2, x, xs, e3) ->
    let (c1,tp1) = collect g e1 in
    let (c2,tp2) = collect g e2 in
    let tA = newvar() in
    let g' = (xs,TyList(TyVar tA))::(x,TyVar tA)::g in
    let (c3,tp3) = collect g' e3 in
    (c1@c2@c3@[(tp1,TyList(TyVar tA));(tp2,tp3)], tp2)

  | Nothing ->
    ([], TyMaybe (TyVar (newvar())))
      
  | Just e1 ->
      let (c1,tp1) = collect g e1 in
      (c1, TyMaybe tp1)
      
  | MatchMaybe(e1, e2, x, e3) ->
    let (c1,tp1) = collect g e1 in
    let (c2,tp2) = collect g e2 in
    let tA = newvar() in
    let g' = (x,TyVar tA)::g in 
    let (c3,tp3) = collect g' e3 in
    (c1@c2@c3@[(tp1,TyMaybe (TyVar tA));(tp2,tp3)], tp2)

(* substituições *)
                      
type subst = (int*tipo) list


(* para imprimir substituições  *)
                
let rec print_subst (s:subst) =
  match s with
    []       -> print_string "\n";
  | (a,b)::s' -> 
      print_int a;
      print_string " |-> ";
      print_string (tipo_str b);
      print_string "\n";
      print_subst s'
           
(* aplicação de substituição a tipo *)
           
let rec appsubs (s:subst) (tp:tipo) : tipo =
  match tp with
    TyInt           -> TyInt
  |  TyBool          -> TyBool       
  |  TyFn   (t1,t2)  -> TyFn   (appsubs s t1, appsubs s t2)
  |  TyVar  x        -> (match lookup s x with
        None     -> TyVar x
      | Some tp' -> tp')
  |  TyList t1       -> TyList (appsubs s t1)
  |  TyMaybe t1      -> TyMaybe (appsubs s t1)                      

                
(* composição de substituições: s1 o s2  *)
let rec compose (s1:subst) (s2:subst) : subst =
  let r1 = List.map (fun (x,tp) -> (x, appsubs s1 tp))    s2 in
  let (vs,_) = List.split s2                                 in
  let r2 = List.filter (fun (x,y) -> not (List.mem x vs)) s1 in
  r1@r2


(* aplicação de substituição a coleção de constraints *)
                                                              
(* s [ (t11,t12), ....(tn1,tn2]) = [(s t11,s t12), ....(s tn1, s tn2] *) 

let rec appconstr (s:subst) (c:constraints) : constraints =
  List.map (fun (a,b) -> (appsubs s a,appsubs s b)) c

  
(* testa se variável ocorre em tipo *)
                 
let rec var_in_tipo (v:int) (tp:tipo) : bool =
  match tp with
    TyInt           -> false
  |  TyBool          -> false       
  |  TyFn   (tp1,tp2)  -> (var_in_tipo v tp1)||(var_in_tipo v tp2)
  |  TyVar  x        -> v==x
  |  TyList tp1       -> var_in_tipo v tp1      
  |  TyMaybe tp1      -> var_in_tipo v tp1                

(* unificação *)

exception UnifyFail of (tipo*tipo)

let rec unify (c:constraints) : subst =
  match c with
    [] -> []
  | (TyInt,    TyInt )   ::c' -> unify c'
  | (TyBool,   TyBool)   ::c' -> unify c'
  | (TyVar x1, TyVar x2) ::c' when x1==x2 -> unify c'
                             
  | (TyFn(tp1,tp2),  TyFn(tp3,tp4)  )::c' -> unify ((tp1,tp3)::(tp2,tp4)::c')
                                               
  | (TyList tp1,     TyList tp2)     ::c' -> unify ((tp1,tp2)::c')
  
  | (TyMaybe tp1,    TyMaybe tp2)    ::c' -> unify ((tp1,tp2)::c')

  | (TyVar x1, tp2)::c' -> 
      if var_in_tipo x1 tp2
      then raise (UnifyFail(TyVar x1, tp2))
      else compose
          (unify (appconstr [(x1,tp2)] c'))
          [(x1,tp2)]  
      
  | (tp1,TyVar x2)::c'  -> 
      if var_in_tipo x2 tp1
      then raise (UnifyFail(tp1,TyVar x2))
      else compose
          (unify (appconstr [(x2,tp1)] c'))
          [(x2,tp1)] 

  | (tp1,tp2)::c' -> raise (UnifyFail(tp1,tp2))




(* INFERÊNCIA DE TIPOS - CHAMADA PRINCIPAL *)
       
let type_infer (e:expr) : unit =
  print_string "\nexpr:\n";
  print_string (expr_str e);
  print_string "\n\n";
  try
    let (c,tp) = collect [] e  in
    let s      = unify c       in
    let tf     = appsubs s tp  in
    print_string "\nRestrições:\n";
    print_constr c;
    print_string "Tipo inferido: ";    
    print_string (tipo_str tp);
    print_string "\n\nSubstituição:\n";
    print_subst s;
    print_string "Tipo inferido (após subs): ";
    print_string (tipo_str tf);
    print_string "\n\n"

  with
    
  | CollectFail x   -> print_string "Erro: variável ";
      print_string x;
      print_string "não declarada!\n\n"
                     
  | UnifyFail (tp1,tp2) -> print_string "Erro: impossível unificar os tipos\n* ";
      print_string (tipo_str tp1);
      print_string "\n* ";
      print_string (tipo_str tp2);
      print_string "\n\n"
                                              
(*********   EXEMPLOS DE CÓDIGO  ***********)

