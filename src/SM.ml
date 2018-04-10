open GT       
open Language
open List
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string list * string list
(* end procedure definition        *) | END
(* calls a procedure               *) | CALL  of string with show
                             
(* The type for the stack machine program *)                                                               
type prg = insn list
                            
(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         

let checkConditionalJump condition value = match condition with
| "nz" -> value <> 0
| "z" -> value = 0

let rec resolveArgumentsMapping accumulator args stack = match args, stack with
  | [], _ -> List.rev accumulator, stack
  | a::tlArgs, s::tlStack -> resolveArgumentsMapping ((a, s)::accumulator) tlArgs tlStack

let rec eval env ((cstack, stack, ((st, i, o) as c)) as conf) = function
| [] -> conf
| insn :: prg' ->
     (match insn with
      | BINOP op -> let y::x::stack' = stack in eval env (cstack, Expr.to_func op x y :: stack', c) prg'
      | READ     -> let z::i' = i     in eval env (cstack, z::stack, (st, i', o)) prg'
      | WRITE    -> let z::stack' = stack in eval env (cstack, stack', (st, i, o @ [z])) prg'
      | CONST i  -> eval env (cstack, i::stack, c) prg'
      | LD x     -> eval env (cstack, State.eval st x :: stack, c) prg'
      | ST x     -> let z::stack' = stack in eval env (cstack, stack', (State.update x z st, i, o)) prg'
      | LABEL s  -> eval env conf prg'
      | JMP name -> eval env conf (env#labeled name)
      | CJMP (condition, name) -> eval env conf (if (checkConditionalJump condition (hd stack)) then (env#labeled name) else prg')
      | CALL f -> eval env ((prg', st)::cstack, stack, c) (env#labeled f)
      | BEGIN (args, locals) -> let resolvedArgumentsMapping, newStack = resolveArgumentsMapping [] args stack 
        in eval env (cstack, newStack, (List.fold_left (fun s (x, v) -> State.update x v s) (State.enter st (args @ locals)) resolvedArgumentsMapping, i, o)) prg'
      | END -> (match cstack with
        | (prg', st')::cstack' -> eval env (cstack', stack, (State.leave st st', i, o)) prg'
        | [] -> conf)
     )

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  (*print_prg p;*)
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, _, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
class labels = 
  object (self)
    val counter = 0
    method new_label = "label_" ^ string_of_int counter, {<counter = counter + 1>}
  end  

let funcLabel name = "L" ^ name

let rec compileWithLabels labels =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  | Expr.Call (f, args)   -> compileCall f args
  and compileCall f args = let compiledArgsList = List.map expr (List.rev args) in
                                    let compiledArgs = List.concat compiledArgsList in
                                    compiledArgs @ [CALL (funcLabel f)]
  in
  function
  | Stmt.Seq (s1, s2)  -> 
    let labels1, res1 = compileWithLabels labels s1 in
    let labels2, res2 = compileWithLabels labels1 s2 in
    labels2, res1 @ res2
  | Stmt.Read x        -> labels, [READ; ST x]
  | Stmt.Write e       -> labels, expr e @ [WRITE]
  | Stmt.Assign (x, e) -> labels, expr e @ [ST x]
  | Stmt.Skip          -> labels, []
  | Stmt.If (condition, ifAction, elseAction) ->
    let compiledCondition = expr condition in
    let jumpElse, labels1 = labels#new_label in
    let jumpEndIf, labels2 = labels1#new_label in
    let labels3, compiledIf = compileWithLabels labels2 ifAction in
    let labels4, compiledElse = compileWithLabels labels3 elseAction in
    labels4, compiledCondition @ [CJMP ("z", jumpElse)] @ compiledIf @ [JMP jumpEndIf] @ [LABEL jumpElse] @ compiledElse @ [LABEL jumpEndIf]
  | Stmt.While (condition, loopAction) ->
    let compiledCondition = expr condition in
    let labelBegin, labels1 = labels#new_label in
    let labelEnd, labels2 = labels1#new_label in
    let labels3, compiledLoopAction = compileWithLabels labels2 loopAction in
    labels3, [LABEL labelBegin] @ compiledCondition @ [CJMP ("z", labelEnd)] @ compiledLoopAction @ [JMP labelBegin] @ [LABEL labelEnd] 
  | Stmt.Repeat (loopAction, condition) ->
    let compiledCondition = expr condition in
    let labelBegin, labels1 = labels#new_label in
    let labels2, compiledLoopAction = compileWithLabels labels1 loopAction in
    labels2, [LABEL labelBegin] @ compiledLoopAction @ compiledCondition @ [CJMP ("z", labelBegin)]
  | Stmt.Call (f, args) -> labels, compileCall f args
  | Stmt.Return res -> labels, (match res with None -> [] | Some v -> expr v) @ [END]

let compileFuncDefinition labels (name, (args, locals, body)) = let endLabel, labels1 = labels#new_label in
  let labels2, compiledFunction = compileWithLabels labels1 body in
  labels2, [LABEL name; BEGIN (args, locals)] @ compiledFunction @ [LABEL endLabel; END]

let compileAllFuncDefinitions labels defs = 
  List.fold_left (fun (labels, allDefsCode) (name, others) -> let labels1, singleCode = compileFuncDefinition labels (funcLabel name, others) in labels1, singleCode::allDefsCode)
    (labels, []) defs

let compile (defs, p) = let endLabel, labels = (new labels)#new_label in
  let labels1, compiledProgram = compileWithLabels labels p in 
  let labels2, allFuncDefinitions = compileAllFuncDefinitions labels1 defs in
  (LABEL "main" :: compiledProgram @ [LABEL endLabel]) @ [END] @ (List.concat allFuncDefinitions)
