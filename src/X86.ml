(* X86 codegeneration interface *)

(* The registers: *)
let regs = [|"%ebx"; "%ecx"; "%esi"; "%edi"; "%eax"; "%edx"; "%ebp"; "%esp"|]

(* We can not freely operate with all register; only 3 by now *)                    
let num_of_regs = Array.length regs - 5

(* We need to know the word size to calculate offsets correctly *)
let word_size = 4

(* We need to distinguish the following operand types: *)
type opnd = 
| R of int     (* hard register                    *)
| S of int     (* a position on the hardware stack *)
| M of string  (* a named memory location          *)
| L of int     (* an immediate operand             *)

(* For convenience we define the following synonyms for the registers: *)         
let ebx = R 0
let ecx = R 1
let esi = R 2
let edi = R 3
let eax = R 4
let edx = R 5
let ebp = R 6
let esp = R 7

(* Now x86 instruction (we do not need all of them): *)
type instr =
(* copies a value from the first to the second operand  *) | Mov   of opnd * opnd
(* makes a binary operation; note, the first operand    *) | Binop of string * opnd * opnd
(* designates x86 operator, not the source language one *)
(* x86 integer division, see instruction set reference  *) | IDiv  of opnd
(* see instruction set reference                        *) | Cltd
(* sets a value from flags; the first operand is the    *) | Set   of string * string
(* suffix, which determines the value being set, the    *)                     
(* the second --- (sub)register name                    *)
(* pushes the operand on the hardware stack             *) | Push  of opnd
(* pops from the hardware stack to the operand          *) | Pop   of opnd
(* call a function by a name                            *) | Call  of string
(* returns from a function                              *) | Ret
(* a label in the code                                  *) | Label of string
(* a conditional jump                                   *) | CJmp  of string * string
(* a non-conditional jump                               *) | Jmp   of string
                                                               
(* Instruction printer *)
let show instr =
  let binop = function
  | "+"   -> "addl"
  | "-"   -> "subl"
  | "*"   -> "imull"
  | "&&"  -> "andl"
  | "!!"  -> "orl" 
  | "^"   -> "xorl"
  | "cmp" -> "cmpl"
  | _     -> failwith "unknown binary operator"
  in
  let opnd = function
  | R i -> regs.(i)
  | S i -> Printf.sprintf "-%d(%%ebp)" ((i+1) * word_size)
  | M x -> x
  | L i -> Printf.sprintf "$%d" i
  in
  match instr with
  | Cltd               -> "\tcltd"
  | Set   (suf, s)     -> Printf.sprintf "\tset%s\t%s"     suf s
  | IDiv   s1          -> Printf.sprintf "\tidivl\t%s"     (opnd s1)
  | Binop (op, s1, s2) -> Printf.sprintf "\t%s\t%s,\t%s"   (binop op) (opnd s1) (opnd s2)
  | Mov   (s1, s2)     -> Printf.sprintf "\tmovl\t%s,\t%s" (opnd s1) (opnd s2)
  | Push   s           -> Printf.sprintf "\tpushl\t%s"     (opnd s)
  | Pop    s           -> Printf.sprintf "\tpopl\t%s"      (opnd s)
  | Ret                -> "\tret"
  | Call   p           -> Printf.sprintf "\tcall\t%s" p
  | Label  l           -> Printf.sprintf "%s:\n" l
  | Jmp    l           -> Printf.sprintf "\tjmp\t%s" l
  | CJmp  (s , l)      -> Printf.sprintf "\tj%s\t%s" s l

(* Opening stack machine to use instructions without fully qualified names *)
open SM

(* Symbolic stack machine evaluator

     compile : env -> prg -> env * instr list

   Take an environment, a stack machine program, and returns a pair --- the updated environment and the list
   of x86 instructions
*)
let inMemory x = match x with
  | S _ -> true
  | M _ -> true
  | _ -> false

let movWithMemoryCheck a b = if inMemory a && inMemory b then [Mov(a, eax); Mov(eax, b)] else [Mov(a, b)]

let compileBinaryOperation op a b = if inMemory a && inMemory b then [Mov (b, eax); Binop (op, a, eax); Mov (eax, b)] else [Binop (op, a, b)]

let getCompareOperationSuffix op = match op with
  | "<" -> "l"
  | ">" -> "g"
  | "<=" -> "le"
  | ">=" -> "ge"
  | "==" -> "e"
  | "!=" -> "ne"

let getDivideRegister op = match op with
  | "/" -> eax
  | "%" -> edx

let compileBinop env op =  let oper1, oper2, updatedEnv = env#pop2 in
  let res, updatedEnv = updatedEnv#allocate in
  let asmInstructions = match op with
    | "+" | "-" | "*" -> (compileBinaryOperation op oper1 oper2) @ (movWithMemoryCheck oper2 res)
    | "&&" | "!!" -> [Mov (L 0, eax); Mov (L 0, edx);
                      Binop ("cmp", L 0, oper1); Set ("nz", "%al"); 
                      Binop ("cmp", L 0, oper2); Set ("nz", "%dl"); 
                      Binop (op, eax, edx); Mov (edx, res)]
    | "<" | ">" | "<=" | ">=" | "==" | "!=" -> let suffix = getCompareOperationSuffix op
      in (compileBinaryOperation "cmp" oper1 oper2) @ [Mov (L 0, eax); Set (suffix, "%al"); Mov (eax, res)]
    | "/" | "%" -> let divideRegister = getDivideRegister op
      in [Mov (oper2, eax); Cltd; IDiv oper1; Mov (divideRegister, res)]
    in updatedEnv, asmInstructions

let compileSingleOperation env oper = match oper with
  | CONST n -> 
    let s, updatedEnv = env#allocate in 
    let asmInstructions = [Mov (L n, s)]
    in updatedEnv, asmInstructions
  | READ -> 
    let s, updatedEnv = env#allocate in 
    let asmInstructions = [Call "Lread"; Mov (eax, s)]
    in updatedEnv, asmInstructions 
  | WRITE -> 
    let s, updatedEnv = env#pop in 
    let asmInstructions = [Push s; Call "Lwrite"; Pop eax]
    in updatedEnv, asmInstructions
  | LD x -> 
    let s, updatedEnv = (env#global x)#allocate in
    let mem = M (updatedEnv#loc x) in
    let asmInstructions = movWithMemoryCheck mem s
    in updatedEnv, asmInstructions   
  | ST x -> 
    let s, updatedEnv = (env#global x)#pop in
    let mem = M (updatedEnv#loc x) in
    let asmInstructions = movWithMemoryCheck s mem
    in updatedEnv, asmInstructions
  | BINOP op -> compileBinop env op
  | LABEL label -> env, [Label label]
  | JMP label -> env, [Jmp label]
  | CJMP (condition, label) -> 
    let s, updatedEnv = env#pop in
    let asmInstructions = [Binop ("cmp", L 0, s); CJmp (condition, label)]
    in updatedEnv, asmInstructions

let rec compile env code = match code with
  | headInstruction::tailCode -> 
    let updatedEnv, instruction = compileSingleOperation env headInstruction in
    let resultEnv, instructions = compile updatedEnv tailCode in resultEnv, instruction @ instructions
  | [] -> env, []

(* A set of strings *)           
module S = Set.Make (String)

(* Environment implementation *)
class env =
  object (self)
    val stack_slots = 0        (* maximal number of stack positions *)
    val globals     = S.empty  (* a set of global variables         *)
    val stack       = []       (* symbolic stack                    *)

    (* gets a name for a global variable *)
    method loc x = "global_" ^ x                                 

    (* allocates a fresh position on a symbolic stack *)
    method allocate =    
      let x, n =
	let rec allocate' = function
	| []                            -> ebx     , 0
	| (S n)::_                      -> S (n+1) , n+1
	| (R n)::_ when n < num_of_regs -> R (n+1) , stack_slots
        | (M _)::s                      -> allocate' s
	| _                             -> S 0     , 1
	in
	allocate' stack
      in
      x, {< stack_slots = max n stack_slots; stack = x::stack >}

    (* pushes an operand to the symbolic stack *)
    method push y = {< stack = y::stack >}

    (* pops one operand from the symbolic stack *)
    method pop  = let x::stack' = stack in x, {< stack = stack' >}

    (* pops two operands from the symbolic stack *)
    method pop2 = let x::y::stack' = stack in x, y, {< stack = stack' >}

    (* registers a global variable in the environment *)
    method global x  = {< globals = S.add ("global_" ^ x) globals >}

    (* gets the number of allocated stack slots *)
    method allocated = stack_slots

    (* gets all global variables *)      
    method globals = S.elements globals
  end

(* Compiles a unit: generates x86 machine code for the stack program and surrounds it
   with function prologue/epilogue
*)
let compile_unit env scode =  
  let env, code = compile env scode in
  env, 
  ([Push ebp; Mov (esp, ebp); Binop ("-", L (word_size*env#allocated), esp)] @ 
   code @
   [Mov (ebp, esp); Pop ebp; Binop ("^", eax, eax); Ret]
  )

(* Generates an assembler text for a program: first compiles the program into
   the stack code, then generates x86 assember code, then prints the assembler file
*)
let genasm prog =
  let env, code = compile_unit (new env) (SM.compile prog) in
  let asm = Buffer.create 1024 in
  Buffer.add_string asm "\t.data\n";
  List.iter
    (fun s ->
       Buffer.add_string asm (Printf.sprintf "%s:\t.int\t0\n" s)
    )
    env#globals;
  Buffer.add_string asm "\t.text\n";
  Buffer.add_string asm "\t.globl\tmain\n";
  Buffer.add_string asm "main:\n";
  List.iter
    (fun i -> Buffer.add_string asm (Printf.sprintf "%s\n" @@ show i))
    code;
  Buffer.contents asm

(* Builds a program: generates the assembler file and compiles it with the gcc toolchain *)
let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  Printf.fprintf outf "%s" (genasm stmt);
  close_out outf;
  let inc = try Sys.getenv "RC_RUNTIME" with _ -> "../runtime" in
  Sys.command (Printf.sprintf "gcc -m32 -o %s %s/runtime.o %s.s" name inc name)
 
