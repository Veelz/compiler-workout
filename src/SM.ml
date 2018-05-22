open GT       
open Language
open List
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP   of string
(* put a constant on the stack     *) | CONST   of int
(* put a string on the stack       *) | STRING  of string
(* create an S-expression          *) | SEXP    of string * int
(* load a variable to the stack    *) | LD      of string
(* store a variable from the stack *) | ST      of string
(* store in an array               *) | STA     of string * int
(* a label                         *) | LABEL   of string
(* unconditional jump              *) | JMP     of string
(* conditional jump                *) | CJMP    of string * string
(* begins procedure definition     *) | BEGIN   of string * string list * string list
(* end procedure definition        *) | END
(* calls a function/procedure      *) | CALL    of string * int * bool
(* returns from a function         *) | RET     of bool
(* drops the top element off       *) | DROP
(* duplicates the top element      *) | DUP
(* swaps two top elements          *) | SWAP
(* checks the tag of S-expression  *) | TAG     of string
(* enters a scope                  *) | ENTER   of string list
(* leaves a scope                  *) | LEAVE
with show
                                                   
(* The type for the stack machine program *)
type prg = insn list

let print_prg p = List.iter (fun i -> Printf.printf "%s\n" (show(insn) i)) p
                            
(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
*)
type config = (prg * State.t) list * Value.t list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                                                  
let split n l =
  let rec unzip (taken, rest) = function
  | 0 -> (List.rev taken, rest)
  | n -> let h::tl = rest in unzip (h::taken, tl) (n-1)
  in
  unzip ([], l) n
          
let checkConditionalJump condition value = match condition with
  | "nz" -> Value.to_int value <> 0
  | "z" -> Value.to_int value = 0

let rec resolveArgumentsMapping accumulator args stack = match args, stack with
  | [], _ -> List.rev accumulator, stack
  | a::tlArgs, s::tlStack -> resolveArgumentsMapping ((a, s)::accumulator) tlArgs tlStack
        
let rec eval env ((cstack, stack, ((st, i, o) as c)) as conf) = function
| [] -> conf
| insn :: prg' ->
     (match insn with
      | BINOP op -> let y::x::stack' = stack in eval env (cstack, (Value.of_int (Expr.to_func op (Value.to_int x) (Value.to_int y))) :: stack', c) prg'
      | CONST i  -> eval env (cstack, (Value.of_int i)::stack, c) prg'
      | STRING s -> eval env (cstack, (Value.of_string s)::stack, c) prg'
      | SEXP (s, n) -> let v, newStack = split n stack in eval env (cstack, (Value.sexp s (List.rev v))::newStack, c) prg'
      | LD x     -> eval env (cstack, State.eval st x :: stack, c) prg'
      | ST x     -> let z::stack' = stack in eval env (cstack, stack', (State.update x z st, i, o)) prg'
      | STA (x, n) -> let v::ind, stack' = split (n + 1) stack in eval env (cstack, stack', (Language.Stmt.update st x v (List.rev ind), i, o)) prg'
      | LABEL s  -> eval env conf prg'
      | JMP name -> eval env conf (env#labeled name)
      | CJMP (condition, name) -> eval env (cstack, tl stack, c) (if (checkConditionalJump condition (hd stack)) then (env#labeled name) else prg')
      | CALL (f, n, p) -> if env#is_label f then eval env ((prg', st)::cstack, stack, c) (env#labeled f) else eval env (env#builtin conf f n p) prg'
      | BEGIN (_, args, locals) -> let resolvedArgumentsMapping, newStack = resolveArgumentsMapping [] args stack 
        in eval env (cstack, newStack, (List.fold_left (fun s (x, v) -> State.update x v s) (State.enter st (args @ locals)) resolvedArgumentsMapping, i, o)) prg'
      | END | RET _ -> (match cstack with
        | (prg', st')::cstack' -> eval env (cstack', stack, (State.leave st st', i, o)) prg'
        | [] -> conf)
      | DROP -> eval env (cstack, List.tl stack, c) prg'
      | DUP -> eval env (cstack, List.hd stack :: stack, c) prg'
      | SWAP -> let fst::snd::st1 = stack in eval env (cstack, snd::fst::st1, c) prg'
      | TAG t -> let fst::st1 = stack in eval env (cstack, (Value.of_int (match fst with Value.Sexp (t', _) when t' = t -> 1 | _ -> 0))::st1, c) prg'
      | ENTER xs -> let vals, st1 = split (List.length xs) stack in let combined = List.combine xs vals in eval env (cstack, st1, (State.push st (List.fold_left (fun s (x, v) -> State.bind x v s) State.undefined combined) xs, i, o)) prg'
      | LEAVE -> eval env (cstack, stack, (State.drop st, i, o)) prg'
     )

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  (* print_prg p; *)
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, _, (_, _, o)) =
    eval
      (object
         method is_label l = M.mem l m
         method labeled l = M.find l m
         method builtin (cstack, stack, (st, i, o)) f n p =
           let f = match f.[0] with 'L' -> String.sub f 1 (String.length f - 1) | _ -> f in
           let args, stack' = split n stack in
           let (st, i, o, r) = Language.Builtin.eval (st, i, o, None) (List.rev args) f in
           let stack'' = if p then stack' else let Some r = r in r::stack' in
           Printf.printf "Builtin: %s\n";
           (cstack, stack'', (st, i, o))
       end
      )
      ([], [], (State.empty, i, []))
      p
  in
  o

class labels = 
  object (self)
    val counter = 0
    method new_label = "label_" ^ string_of_int counter, {<counter = counter + 1>}
  end 

let funcLabel name = "L" ^ name

let rec compileWithLabels ll labels =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.String s         -> [STRING s]
  | Expr.Array arr        -> List.flatten (List.map expr arr) @ [CALL ("$array", List.length arr, false)]
  | Expr.Elem (arr, i)    -> expr arr @ expr i @ [CALL ("$elem", 2, false)]
  | Expr.Length t         -> expr t @ [CALL ("$length", 1, false)]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  | Expr.Call (f, args)   -> compileCall f args false
  | Expr.Sexp (t, xs)     -> List.flatten (List.map expr xs) @ [SEXP (t, List.length xs)]
  and compileCall f args p = let compiledArgsList = List.map expr (List.rev args) in
                                    let compiledArgs = List.concat compiledArgsList in
                                    compiledArgs @ [CALL (funcLabel f, List.length args, p)]
  and pattern lf = function
  | Stmt.Pattern.Wildcard         -> [DROP]
  | Stmt.Pattern.Sexp     (t, ps) -> [DUP; TAG t; CJMP ("z", lf)] @ (List.concat (List.mapi (fun i p -> [DUP; CONST i; CALL (".elem", 2, false)] @ pattern lf p) ps))
  | Stmt.Pattern.Ident     n      -> [DROP]
  and bindings p =
    let rec inner = function
    | Stmt.Pattern.Ident n      -> [SWAP]
    | Stmt.Pattern.Sexp (_, ps) -> (List.flatten (List.mapi (fun i p -> [DUP; CONST i; CALL (".elem", 2, false)] @ inner p) ps)) @ [DROP]
    | Stmt.Pattern.Wildcard     -> [DROP]
    in
    inner p @ [ENTER (Stmt.Pattern.vars p)]
  in
  function
  | Stmt.Seq (s1, s2)  -> 
    let l2, labels = labels#new_label in
    let labels1, res1 = compileWithLabels l2 labels s1 in
    let labels2, res2 = compileWithLabels ll labels1 s2 in
    labels2, res1 @ [LABEL l2] @ res2
  | Stmt.Assign (x, [], e) -> labels, expr e @ [ST x]
  | Stmt.Assign (x, is, e) -> labels, List.flatten (List.map expr (is @ [e])) @ [STA (x, List.length is)]
  | Stmt.Skip          -> labels, []
  | Stmt.If (condition, ifAction, elseAction) ->
    let compiledCondition = expr condition in
    let jumpElse, labels1 = labels#new_label in
    let jumpEndIf, labels2 = labels1#new_label in
    let labels3, compiledIf = compileWithLabels ll labels2 ifAction in
    let labels4, compiledElse = compileWithLabels ll labels3 elseAction in
    labels4, compiledCondition @ [CJMP ("z", jumpElse)] @ compiledIf @ [JMP jumpEndIf] @ [LABEL jumpElse] @ compiledElse @ [LABEL jumpEndIf]
  | Stmt.While (condition, loopAction) ->
    let compiledCondition = expr condition in
    let labelBegin, labels1 = labels#new_label in
    let labelEnd, labels2 = labels1#new_label in
    let labels3, compiledLoopAction = compileWithLabels labelEnd labels2 loopAction in
    labels3, [LABEL labelBegin] @ compiledCondition @ [CJMP ("z", labelEnd)] @ compiledLoopAction @ [JMP labelBegin] @ [LABEL labelEnd] 
  | Stmt.Repeat (loopAction, condition) ->
    let compiledCondition = expr condition in
    let labelBegin, labels1 = labels#new_label in
    let labelEnd, labels1 = labels#new_label in
    let labels2, compiledLoopAction = compileWithLabels labelEnd labels1 loopAction in
    labels2, [LABEL labelBegin] @ compiledLoopAction @ [LABEL labelEnd] @ compiledCondition @ [CJMP ("z", labelBegin)]
  | Stmt.Call (f, args) -> labels, compileCall f args true
  | Stmt.Return res -> labels, (match res with None -> [] | Some v -> expr v) @ [RET (res <> None)]
  | Stmt.Leave -> labels, [LEAVE]
  | Stmt.Case (e, [p, s]) ->
    let patternCode = pattern ll p in
    let labels1, statementCode = compileWithLabels ll labels (Stmt.Seq (s, Stmt.Leave)) in
    labels1, expr e @ patternCode @ bindings p @ statementCode
  | Stmt.Case (e, branches) ->
    let n = List.length branches - 1 in
    let labels2, _, _, code = 
      List.fold_left (fun (env1, labelss, i, code) (p, s) -> let (lf, env), jmp = if i = n then (ll, env1), [] else labels#new_label, [JMP ll] in 
        let patternCode = pattern lf p in let labels1, statementCode = compileWithLabels ll labels (Stmt.Seq (s, Stmt.Leave)) in  
        (labels1, Some lf, i + 1, ((match labelss with None -> [] | Some l -> [LABEL l]) @ patternCode @ bindings p @ statementCode @ jmp) :: code)) (labels, None, 0, []) branches
    in labels2, expr e @ (List.flatten (List.rev code))

let compileFuncDefinition labels (name, (args, locals, body)) = let endLabel, labels1 = labels#new_label in
  let labels2, compiledFunction = compileWithLabels endLabel labels1 body in
  labels2, [LABEL name; BEGIN (name, args, locals)] @ compiledFunction @ [LABEL endLabel; END]

let compileAllFuncDefinitions labels defs = 
  List.fold_left (fun (labels, allDefsCode) (name, others) -> let labels1, singleCode = compileFuncDefinition labels (funcLabel name, others) in labels1, singleCode::allDefsCode)
    (labels, []) defs

(* Stack machine compiler
     val compile : Language.t -> prg
   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let compile (defs, p) = let endLabel, labels = (new labels)#new_label in
  let labels1, compiledProgram = compileWithLabels endLabel labels p in 
  let labels2, allFuncDefinitions = compileAllFuncDefinitions labels1 defs in
  (compiledProgram @ [LABEL endLabel]) @ [END] @ (List.concat allFuncDefinitions)