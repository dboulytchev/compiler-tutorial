type expr =
  | Const of int
  | Var   of string
  | Add   of expr * expr
  | Mul   of expr * expr

let rec eval (state : string -> int) expr =
  match expr with
  | Const n     -> n
  | Var   x     -> state x
  | Add  (l, r) -> eval state l + eval state r
  | Mul  (l, r) -> eval state l * eval state r
  
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
  | S_ADD
  | S_MUL

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
          | S_ADD ->
             let x::y::stack' = stack in
             (state, (y+x)::stack', input, output)
          | S_MUL ->
             let x::y::stack' = stack in
             (state, (y*x)::stack', input, output)
         )
         code'
  in
  srun' ([], [], input, []) code 

let rec compile_expr expr =
  match expr with
  | Const n     -> [S_PUSH n]
  | Var   x     -> [S_LD x]
  | Add  (l, r) -> compile_expr l @ compile_expr r @ [S_ADD]
  | Mul  (l, r) -> compile_expr l @ compile_expr r @ [S_MUL]

let rec compile_stmt stmt =
  match stmt with
  | Skip          -> []
  | Read    x     -> [S_READ; S_ST x]
  | Write   e     -> compile_expr e @ [S_WRITE]
  | Assign (x, e) -> compile_expr e @ [S_ST x]
  | Seq    (l, r) -> compile_stmt l @ compile_stmt r
                                                       
