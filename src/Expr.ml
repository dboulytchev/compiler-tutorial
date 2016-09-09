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
