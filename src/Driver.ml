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

