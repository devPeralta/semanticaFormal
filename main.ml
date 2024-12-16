
type tipo = 
  | TyInt 
  | TyBool
  | TyFn of tipo * tipo
  | TyList of tipo (*Tipo lista*)

type bop = Sum | Sub | Mul | Div 
         | Eq | Neq | Leq | Lt | Geq | Gt 
         | And | Or

type expr = 
  | Num of int
  | Bool of bool 
  | If of expr * expr * expr 
  | Bop of bop * expr * expr
  | Id of string
  | Fn of  string * tipo * expr 
  | App of expr * expr
  | Let of string * tipo * expr * expr
  | LetRec of string * tipo * expr * expr 
  | Nil (* expr list*)
  | Head of expr (* expr list*)
  | Tail of expr (* expr list*)


type tyenv = (string * tipo) list 

exception TypeError of string 

exception BugParser
  
let rec typeinfer (g:tyenv) (e:expr) : tipo = 
  match e with
  | Num n -> failwith "não implementado"
      
  | Bool b -> failwith "não implementado"

  | If(e1,e2,e3) -> failwith "não implementado"
  
  | Bop(op, e1,e2) -> failwith "não implementado"
      
  | Id x  -> failwith "não implementado"
      
  | Fn(x,t,e1) -> failwith "não implementado"
  
  | App(e1,e2) -> failwith "não implementado"
      
  | Let(x,t,e1,e2) -> failwith "não implementado"
      
  | LetRec(f, TyFn(t,t'), Fn(x,t'',e1), e2) -> failwith "não implementado"
  
  | LetRec _ -> failwith "não implementado"

  | Nil -> "[]" (* expr list*)

  | Head e1 -> "hd " ^ (expr_str e1) (* expr list*)
  
  | Tail e1 -> "tl " ^ (expr_str e1) (* expr list*)
  

(* ===========  avaliação ===============*)

let rec isvalue (e:expr) : bool = 
  match e with
  | Num _ | Bool _ | Fn _ -> true 
  | _  -> false 

exception NoRuleApplies

exception BugTypeInfer

let rec subs v x e = 
  match e with
  | Num _ | Bool _ -> failwith "não implementado"

  | If(e1,e2,e3) -> failwith "não implementado"

  | Bop(op,e1,e2) -> failwith "não implementado"

  | Id y -> failwith "não implementado"  

  | Fn(y,t,e1) when x = y -> failwith "não implementado"
  | Fn(y,t,e1)            -> failwith "não implementado"
  
  | App(e1,e2) -> failwith "não implementado"

  | Let(y,t,e1,e2) when x = y ->  failwith "não implementado" 
  | Let(y,t,e1,e2)            ->  failwith "não implementado" 
  
  | LetRec(f, tf, Fn(y,t'',e1), e2) when x = f -> failwith "não implementado"
  | LetRec(f, tf, Fn(y,t'',e1), e2)            -> failwith "não implementado"  
      
  | LetRec _ -> raise BugParser

   

   

let compute (op,v1,v2) = 
  match (v1,v2) with
  | (Num n1, Num n2) ->
      (match op with
       | Sum -> Num (n1 + n2)
       | Sub -> Num (n1 - n2)
       | Mul -> Num (n1 * n2)
       | Div -> Num (n1 / n2)
       | Eq  -> Bool (n1 = n2)
       | Neq -> Bool (n1 != n2)
       | Leq -> Bool (n1 <= n2)
       | Lt -> Bool (n1 < n2)
       | Geq -> Bool (n1 >= n2)
       | Gt -> Bool (n1 > n2)
       | _ -> raise BugTypeInfer)
      
  | (Bool b1, Bool b2) ->
      (match op with 
       | And -> Bool (b1 && b2)
       | Or -> Bool (b1 || b2)
       | _ -> raise BugTypeInfer)
      
  | _ -> raise BugTypeInfer

let rec step (e:expr) : expr = 
  match e with 
  | Num _ | Bool _ | Fn _ | Id _ -> failwith "não implementado" 

  | Bop (op,v1,v2) when isvalue v1 && isvalue v2 -> failwith "não implementado"
  | Bop (op,v1,e2) when isvalue v1 ->  failwith "não implementado"
  | Bop (op,e1,e2)                 ->  failwith "não implementado"

  | If(Bool true,e2,e3)  -> failwith "não implementado"
  | If(Bool false,e2,e3) -> failwith "não implementado"
  | If(e1,e2,e3)         -> failwith "não implementado"

  | App(Fn(x,t,e1), v) when isvalue v  -> failwith "não implementado"
  | App(v1,e2)         when isvalue v1 -> failwith "não implementado"
  | App(e1,e2)                         -> failwith "não implementado"

  | Let(x,t,v,e2)  when isvalue v -> failwith "não implementado"
  | Let(x,t,e1,e2)                -> failwith "não implementado"

  | LetRec(f, TyFn(t,t'), Fn(x,t'',e1), e2) -> failwith "não implementado"

  | LetRec _ -> failwith "não implementado"
   


let rec eval (e:expr) : expr = 
  try 
    let e' = step e in eval e' 
  with 
    NoRuleApplies -> e


(* ====== interpretador =======*)

let rec strofexpr(v:expr) : string  = 
  match v with 
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Fn _ -> "fun"
  | _ -> failwith "não implementado ainda"

let rec stroftipo(t:tipo) : string = 
  match t with 
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFn(t1,t2) ->  (stroftipo t1) ^ " -> " ^(stroftipo t2)

(* let parse (s : string) : expr =
      let lexbuf = Lexing.from_string s in
      let ast = Parser.prog Lexer.read lexbuf in
      ast 
*)

let inter (e:expr) : unit = 
      (*let e : expr = parse s in *)
  let t : tipo = typeinfer [] e in
  let v : expr = eval e in 
  print_string ((strofexpr v) ^ ":" ^  (stroftipo t) ^ "\n" )




                (*  ===== ASTs para teste  =====*)
(* 
let dobro : int --> int = fn x:int => 2 * x
in  dobro 10  
*)

let tst1 = Let("dobro", TyFn(TyInt,TyInt), Fn("x", TyInt, Bop(Mul, Id "x", Num 2)),
               App(Id "dobro", Num 10))
    
    
(* 
   let rec fat : int -> int = fn (x:int) => if x = 0 then 1 else x * (fat (x - 1)) 
in fat 5 
*) 
let tst2  = LetRec("fat", TyFn(TyInt,TyInt), 
                   Fn("x", TyInt, 
                      If(Bop(Eq,Id "x",Num 0), 
                         Num 1, 
                         Bop(Mul, Id "x", App(Id "fat", Bop(Sub, Id "x", Num 1))))),
                   App(Id "fat", Num 5)) 
                                         
(*
 let rec twice: (int -> int) -> int -> int = 
            fn f: int -> int => fn x:int => f ( f x))  in 
 twice (fn x:int => x + 1) 10
*)

let tst3  = Let("twice", TyFn(TyFn(TyInt,TyInt), TyFn(TyInt,TyInt)), 
                Fn("f", TyFn(TyInt,TyInt), Fn("x", TyInt, App(Id "f", App(Id "f", Id "x")))),
                App(App(Id "twice", Fn("x", TyInt, Bop(Sum, Id "x", Num 1))), Num 10))


                                                                           
