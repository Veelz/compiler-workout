open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let rec eval config prg = 
    let update_config inst ((st, (s, i, o)) : config) = 
        match inst with
          BINOP (binop) -> 
            (match st with 
                  y :: x :: st_end -> ((Syntax.Expr.calc binop x y) :: st_end, (s, i ,o)) 
                | _ -> failwith "Not enough arguments for binary operation")
        | CONST (n) -> (n :: st, (s, i, o))
        | READ -> let num = List.hd i in (num :: st, (s, List.tl i, o))
        | WRITE -> let num = List.hd st in (List.tl st, (s, i, o @ [num]))
        | LD (x) -> ((s x) :: st, (s, i, o))
        | ST (x) -> let num = List.hd st in (List.tl st, (Syntax.Expr.update x num s, i, o)) in
    match prg with
      [] -> config    
    | inst :: tail -> eval (update_config inst config) tail;;

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile (stmt : Syntax.Stmt.t) =
    let rec compile_expr (expr : Syntax.Expr.t) = 
        match expr with
          Const (n) -> [CONST n]
        | Var (x) -> [LD x]
        | Binop (binop, x, y) -> 
            let eval_x = compile_expr x
            and eval_y = compile_expr y in
            eval_x @ eval_y @ [BINOP binop] in
    match stmt with
      Read (x) -> READ :: [ST x]
    | Write (expr) -> (compile_expr expr) @ [WRITE]
    | Assign (x, expr) -> (compile_expr expr) @ [ST x]
    | Seq (stmt_left, stmt_right) -> (compile stmt_left) @ (compile stmt_right);;

