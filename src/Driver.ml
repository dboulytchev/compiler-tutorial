open Expr

(*
read (x);  
  read (y);
    z := x + y;
    write (z*z)
*)

(*
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
  let outf = open_out "sample1.s" in
  Printf.fprintf outf "%s\n" (genasm p);
  close_out outf
*)

let main =
  if Array.length Sys.argv <> 2
  then Printf.printf "Usage: rc <input file.expr>\n"
  else (
    let infile   = Sys.argv.(1) in
    let basename = Filename.chop_suffix infile ".expr" in
    match Parser.parse infile with
    | `Ok stmt -> ignore (Expr.build stmt basename)
    | `Fail er -> Printf.eprintf "Syntac error: %s\n" er
  )
