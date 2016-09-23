type operator =
  | Plus
  | Mult
  | Minus

let to_operator = function
  | "+" -> Plus
  | "*" -> Mult
  | "-" -> Minus
  | _ -> failwith "unknown operator"

type expr =
  | Const of int
  | Var   of string
  | Binop of operator * expr * expr

let to_fun = function
  | Plus  -> (+)
  | Mult  -> ( * )
  | Minus -> (-)

let rec eval (state : string -> int) expr =
  match expr with
  | Const n     -> n
  | Var   x     -> state x
  | Binop (op, l, r) -> (to_fun op) (eval state l) (eval state r)

type stmt =
  | Skip
  | Read   of string
  | Write  of expr
  | Assign of string * expr
  | Seq    of stmt * stmt

let run (input : int list) stmt =
  let rec run' ((state, input, output) as c) stmt =
    let state' x = List.assoc x state in
    match stmt with
    | Skip -> c
    | Seq (l, r) -> run' (run' c l) r
    | Read x ->
       let y::input' = input in
       ((x, y)::state, input', output)
    | Assign (x, e) ->
       ((x, eval state' e)::state, input, output)
    | Write e ->
       (state, input, output @ [eval state' e])
  in
  let (_, _, output) = run' ([], input, []) stmt in
  output

type instr =
  | S_READ
  | S_WRITE
  | S_PUSH  of int
  | S_LD    of string
  | S_ST    of string
  | S_BINOP of operator

let srun (input : int list) (code : instr list) =
  let rec srun' (state, stack, input, output) code =
    match code with
    | [] -> output
    | i::code' ->
       srun'
         (match i with
          | S_READ ->
             let y::input' = input in
             (state, y::stack, input', output)
          | S_WRITE ->
             let y::stack' = stack in
             (state, stack', input, output @ [y])
          | S_PUSH n ->
             (state, n::stack, input, output)
          | S_LD x ->
             (state, (List.assoc x state)::stack, input, output)
          | S_ST x ->
             let y::stack' = stack in
             ((x, y)::state, stack', input, output)
          | S_BINOP op ->
             let x::y::stack' = stack in
             (state, ((to_fun op) y x)::stack', input, output)
         )
         code'
  in
  srun' ([], [], input, []) code

let rec compile_expr expr =
  match expr with
  | Const n     -> [S_PUSH n]
  | Var   x     -> [S_LD x]
  | Binop (op, l, r) -> compile_expr l @ compile_expr r @ [S_BINOP op]

let rec compile_stmt stmt =
  match stmt with
  | Skip          -> []
  | Read    x     -> [S_READ; S_ST x]
  | Write   e     -> compile_expr e @ [S_WRITE]
  | Assign (x, e) -> compile_expr e @ [S_ST x]
  | Seq    (l, r) -> compile_stmt l @ compile_stmt r

type opnd = R of int | S of int | M of string | L of int
let x86regs = [|"%ebx"; "%ecx"; "%esi"; "%edi"; "%eax"; "%edx"|]
let num_of_regs = Array.length x86regs
let word_size = 4
let eax = R 4
let edx = R 5

let allocate env stack =
  match stack with
  | []                                -> R 0
  | (S n)::_                          -> env#allocate (n+2); S (n+1)
  | (R n)::_ when n < num_of_regs - 3 -> R (n+1)
  | _                                 -> env#allocate 1; S 0

type x86instr =
  | X86Binop of operator * opnd * opnd
  | X86Mov   of opnd * opnd
  | X86Push  of opnd
  | X86Pop   of opnd
  | X86Call  of string
  | X86Ret

let x86compile env code =
  let rec x86compile' stack code =
    match code with
    | [] -> []
    | i::code' ->
       let (x86code, stack') =
         match i with
         | S_READ ->
            ([X86Call "read"], [eax])
         | S_WRITE ->
            ([X86Push (R 0); X86Call "write"; X86Pop (R 0)], [])
         | S_LD x ->
            env#local x;
            let s = allocate env stack in
            ([X86Mov (M x, s)], s::stack)
         | S_ST x ->
            env#local x;
            let s::stack' = stack in
            ([X86Mov (s, M x)], stack')
         | S_PUSH n ->
            let s = allocate env stack in
            ([X86Mov (L n, s)], s::stack)
         (* | S_ADD -> *)
         (*    let x::y::stack'= stack in *)
         (*    (match x, y with *)
         (*     | S _, S _ -> *)
         (*       [X86Mov (x, eax); *)
         (*        X86Add (eax, y)], y::stack' *)
         (*     | _ -> *)
         (*       [X86Add (x, y)], y::stack') *)
         | S_BINOP op ->
            let x::y::stack'= stack in
            (match x, y with
             | S _, S _ ->
               [X86Mov       (y, eax);
                X86Binop (op, x, eax);
                X86Mov       (eax, y)], y::stack'
             | _ ->
               [X86Binop (op, x, y)], y::stack')
       in
       x86code @ x86compile' stack' code'
  in
  x86compile' [] code

module S = Set.Make (String)

class env =
  object
    val stack_slots = ref 0
    val locals = ref S.empty
    method allocate n = stack_slots := max n !stack_slots
    method local x = locals := S.add x !locals
    method local_vars = S.elements !locals
    method allocated = !stack_slots
  end

let x86print instr =
  let op_to_x86 = function
    | Plus  -> "addl"
    | Mult  -> "imull"
    | Minus -> "subl"
  in
  let opnd op =
    match op with
    | R i -> x86regs.(i)
    | S i -> Printf.sprintf "-%d(%%ebp)" (i * word_size)
    | M x -> x
    | L i -> Printf.sprintf "$%d" i
  in
  match instr with
  | X86Binop (op, x, y) -> Printf.sprintf "%s\t%s,\t%s" (op_to_x86 op) (opnd x) (opnd y)
  | X86Mov       (x, y) -> Printf.sprintf "movl\t%s,\t%s"  (opnd x) (opnd y)
  | X86Push           x -> Printf.sprintf "pushl\t%s" (opnd x)
  | X86Pop            x -> Printf.sprintf "popl\t%s"  (opnd x)
  | X86Call           f -> Printf.sprintf "call\t%s" f
  | X86Ret              -> "ret"

let genasm stmt =
  let text = Buffer.create 1024 in
  let out  = Buffer.add_string text in
  let env  = new env in
  let code = x86compile env (compile_stmt stmt) in
  out "\t.text\n";
  out "\t.globl\tmain\n";
  List.iter
    (fun x -> out (Printf.sprintf "\t.comm\t%s,\t%d,\t%d\n" x word_size word_size))
    env#local_vars;
  out "main:\n";
  let prologue, epilogue =
    if env#allocated = 0
    then (fun () -> ()), (fun () -> ())
    else
      (fun () ->
         out "\tpushl\t%ebp\n";
         out "\tmovl\t%esp,\t%ebp\n";
         out (Printf.sprintf "\tsubl\t$%d,\t%%esp\n" (env#allocated * word_size))
      ),
      (fun () ->
         out "\tmovl\t%ebp,\t%esp\n";
         out "\tpopl\t%ebp\n"
      )
  in
  prologue ();
  List.iter
    (fun i -> out (Printf.sprintf "\t%s\n" (x86print i)))
    code;
  epilogue();
  out "\txorl\t%eax,\t%eax\n";
  out "\tret\n";
  Buffer.contents text

let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  Printf.fprintf outf "%s" (genasm stmt);
  close_out outf;
  Sys.command (Printf.sprintf "gcc -m32 -o %s ../runtime/runtime.o %s.s" name name)
