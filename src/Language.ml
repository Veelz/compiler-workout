(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT
open List

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
open Ostap
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let rec calc binop x y = 
        let to_bool x = if x = 0 then false else true
        and bool_calc bool_op x y = if bool_op x y then 1 else 0
        in
        match binop with
          "+" -> x + y
        | "-" -> x - y
        | "*" -> x * y
        | "/" -> x / y
        | "%" -> x mod y
        | "<" -> bool_calc (<) x y
        | "<=" -> bool_calc (<=) x y
        | ">" -> bool_calc (>) x y
        | ">=" -> bool_calc (>=) x y
        | "==" -> bool_calc (==) x y
        | "!=" -> bool_calc (<>) x y
        | "&&" -> bool_calc (&&) (to_bool x) (to_bool y)
        | "!!" -> bool_calc (||) (to_bool x) (to_bool y)
        | _ -> failwith "Error in op: %s" binop 

    let rec eval state expression = 
        match expression with
          Const (value) -> value
        | Var (value) -> state value
        | Binop (op, expr1, expr2) ->
            let value1 = eval state expr1 
            and value2 = eval state expr2 
            in calc op value1 value2


    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)
    let parser_binop_list ops = List.map(fun op -> (ostap ($(op)), fun a b -> Binop(op, a, b))) ops

    ostap (
      parse: 
       !(Util.expr
          (fun x -> x)
          [|
            `Lefta, parser_binop_list ["!!"];
            `Lefta, parser_binop_list ["&&"];
            `Nona,  parser_binop_list ["<="; ">="; "<"; ">"; "=="; "!="];
            `Lefta, parser_binop_list ["+"; "-"];
            `Lefta, parser_binop_list ["*"; "/"; "%"]
          |]
          primary
        );
      primary:
          v:IDENT   {Var    v}
        | c:DECIMAL {Const  c}
        | -"(" parse -")"
    )

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval ((s, i, o) : config) stmt =
        match stmt with
          Read (x) -> 
            (match i with 
              [] -> failwith "Empty input stream"
            | num :: tail -> ((Expr.update x num s), tail, o))
        | Write (expr) -> (s, i, o @ [(Expr.eval s expr)])
        | Assign (x, expr) -> ((Expr.update x (Expr.eval s expr) s), i, o)
        | Seq (stmt_fst, stmt_snd) -> 
            let value_fst = eval (s, i, o) stmt_fst in
            eval value_fst stmt_snd;;

    (* Statement parser *)
    ostap (
      parse: 
        !(Ostap.Util.expr
          (fun x -> x)
          [|
            `Righta, [ostap(";"), fun a b -> Seq(a, b)];
          |]
          primary
      );
      primary: 
          -"read" -"(" x:IDENT -")"          {Read  x}
        | -"write" -"(" e:!(Expr.parse) -")" {Write e}
        | x:IDENT -":=" e:!(Expr.parse)      {Assign(x, e)}
    )

  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
