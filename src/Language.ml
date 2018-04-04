(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators
                         
(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let rec contains a lst = match lst with
    | [] -> false
    | x::tl -> if x = a then true else contains a tl

    let empty = let failFun x = failwith (Printf.sprintf "Undefined variable: %s" x)
      in {g = failFun; l = failFun; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s = let updateFun x v s = fun y -> if y = x then v else s y
      in if contains x s.scope then {g = s.g; l = updateFun x v s.l; scope = s.scope} else {g = updateFun x v s.g; l = s.l; scope = s.scope}
                                
    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = if contains x s.scope then (s.l x) else (s.g x)

    (* Creates a new scope, based on a given state *)
    let enter st xs = let emptyState = empty in {g = st.g; l = emptyState.l; scope = xs}

    (* Drops a scope *)
    let leave st st' = {g = st.g; l = st'.l; scope = st'.scope}

  end
    
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
      
    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let to_func op =
      let bti   = function true -> 1 | _ -> 0 in
      let itb b = b <> 0 in
      let (|>) f g   = fun x y -> f (g x y) in
      match op with
      | "+"  -> (+)
      | "-"  -> (-)
      | "*"  -> ( * )
      | "/"  -> (/)
      | "%"  -> (mod)
      | "<"  -> bti |> (< )
      | "<=" -> bti |> (<=)
      | ">"  -> bti |> (> )
      | ">=" -> bti |> (>=)
      | "==" -> bti |> (= )
      | "!=" -> bti |> (<>)
      | "&&" -> fun x y -> bti (itb x && itb y)
      | "!!" -> fun x y -> bti (itb x || itb y)
      | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)    
    
    let rec eval st expr =      
      match expr with
      | Const n -> n
      | Var   x -> State.eval st x
      | Binop (op, x, y) -> to_func op (eval st x) (eval st y)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (                                      
      parse:
    !(Ostap.Util.expr 
             (fun x -> x)
       (Array.map (fun (a, s) -> a, 
                           List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
                        ) 
              [|                
    `Lefta, ["!!"];
    `Lefta, ["&&"];
    `Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
    `Lefta, ["+" ; "-"];
    `Lefta, ["*" ; "/"; "%"];
              |] 
       )
       primary);
      
      primary:
        n:DECIMAL {Const n}
      | x:IDENT   {Var x}
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
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = State.t * int list * int list 

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment supplies the following method

           method definition : string -> (string list, string list, t)

       which returns a list of formal parameters, local variables, and a body for given definition
    *)
    let rec eval env ((st, i, o) as conf) stmt =
      match stmt with
      | Read    x       -> (match i with z::i' -> (State.update x z st, i', o) | _ -> failwith "Unexpected end of input")
      | Write   e       -> (st, i, o @ [Expr.eval st e])
      | Assign (x, e)   -> (State.update x (Expr.eval st e) st, i, o)
      | Seq    (s1, s2) -> eval env (eval env conf s1) s2
      | Skip -> conf
      | If (expr, thenIf, elseIf) -> if (Expr.eval st expr) <> 0 then (eval env conf thenIf) else (eval env conf elseIf)
      | While (expr, loopStmt) -> recursiveWhileLoop env conf expr loopStmt
      | Repeat (loopStmt, expr) ->  recursiveWhileLoop env (eval env conf loopStmt) expr loopStmt
      | Call (f, args) -> let computedArgs = List.map (Expr.eval st) args in
          let formals, locals, body = env#definition f in
          let methodState = State.enter st (formals@locals) in
          let argsMapping = List.combine formals computedArgs in
          let fullMethodState = List.fold_left (fun st (x, a) -> State.update x a st) methodState argsMapping in
          let resultState, resultInput, resultOutput = eval env (fullMethodState, i, o) body in
          (State.leave resultState st, resultInput, resultOutput)
    and recursiveWhileLoop env ((st, _, _) as conf) expr loopStmt = if (Expr.eval st expr) != 0 then recursiveWhileLoop env (eval env conf loopStmt) expr loopStmt else conf

    let rec parseElIfActions elIfActions elseAction =  match elIfActions with
    | [] -> elseAction
    | (condition, action)::tailElIfActions -> If (condition, action, parseElIfActions tailElIfActions elseAction)

    let parseElse elIfActions elseAction = 
      let elseActionParsed = match elseAction with
      | None -> Skip
      | Some action -> action
    in parseElIfActions elIfActions elseActionParsed
              
    (* Statement parser *)
    ostap (
      parse:
        s:stmt ";" ss:parse {Seq (s, ss)}
      | stmt;
      
      stmt:
        "read" "(" x:IDENT ")"          {Read x}
      | "write" "(" e:!(Expr.parse) ")" {Write e}
      | x:IDENT 
        assignmentOrCall: (
          ":=" e:!(Expr.parse)    {Assign (x, e)}
          | "(" args:!(Util.list0)[Expr.parse] ")" {Call (x, args)}
        ) {assignmentOrCall}
      | %"skip"                         {Skip}
      | %"if" condition: !(Expr.parse) %"then" action:parse 
        elIfActions:(%"elif" !(Expr.parse) %"then" parse)*
        elseAction:(%"else" parse)?
        %"fi"                                              { If (condition, action, parseElse elIfActions elseAction)}
      | %"while" condition: !(Expr.parse) %"do" action:parse %"od"  { While (condition, action) }
      | %"repeat" action:parse %"until" condition: !(Expr.parse)    { Repeat (action, condition) }
      | %"for" initialize:parse "," condition: !(Expr.parse)
        "," increment:parse %"do" action:parse %"od"             { Seq (initialize, While (condition, Seq (action, increment))) }
    )
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (
      argument: IDENT;
      parse: %"fun" functionName:IDENT "(" args: !(Util.list0 argument) ")"
        locals: (%"local" !(Util.list argument))?
        "{" body: !(Stmt.parse) "}" { (functionName, (args, (match locals with None -> [] | Some l -> l), body))}
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i = let module CustomMap = Map.Make (String) in
  let definitionsMap = List.fold_left (fun m ((name, _) as definitions) -> CustomMap.add name definitions m) CustomMap.empty defs in
  let envObject = (object method definition name = snd (CustomMap.find name definitionsMap) end) in
  let _, _, output = Stmt.eval envObject (State.empty, i, []) body
  in output
                                   
(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
