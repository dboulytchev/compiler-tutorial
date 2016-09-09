open Expr

(*
read (x);  
  read (y);
    z := x + y;
    write (z*z)
*)

let p =
  Seq (
      Read "x",
      Seq (
          Read "y",
          Seq (
              Assign ("z", Add (Var "x", Var "y")),
              Write (Mul (Var "z", Var "z"))
          )
      )
  )

let _ =
  let [r] = run [4; 5] p in
  Printf.printf "%d\n" r

let run input p = srun input (compile_stmt p)

let _ =
  let [r] = run [4; 5] p in
  Printf.printf "%d\n" r

let (!!) = (!)

let ( ! ) x   = Var x
let ( $ ) n   = Const n
let ( * ) a b = Mul (a, b)
let ( + ) a b = Add (a, b)

let (:=) x e = Assign (x, e)
let skip = Skip
let read x  = Read x
let write e = Write e
let (|>) l r = Seq (l, r)

(*
read (x);  
  read (y);
    z := x + y;
    write (z*z)
*)

let p =
  read "x" |>
  read "y" |>
  ("z" := !"x" + !"y") |>
  write (!"z" * !"z")

let _ =
  let [r] = run [4; 5] p in
  Printf.printf "%d\n" r

let _ =
  List.iter
    (fun i -> Printf.printf "\t%s\n" (x86print i))
    (x86compile (compile_stmt p))
