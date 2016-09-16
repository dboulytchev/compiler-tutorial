open Ostap
open Matcher
open Expr

ostap (
  expr:
    x:mulli "+" y:expr {Add (x, y)}
  | mulli;

  (* mulli: x:primary op:("*" | "/" | "%") y:mulli {Binop (Token.repr op, x, y)} *)
  mulli: x:primary "*" y:mulli {Mul (x, y)}
  | primary;

  primary:
    n:DECIMAL {Const n}
  | x:IDENT   {Var   x}
  | -"(" expr -")"
)

ostap (
  stmt:
    s:simple ";" d:stmt {Seq (s, d)}
  | simple;
  simple:
    x:IDENT ":=" e:expr      {Assign (x, e)}
  | %"read"  "(" x:IDENT ")" {Read x}
  | %"write" "(" e:expr  ")" {Write e}
  | %"skip"                  {Skip}
)

let parse infile =
  let s = Util.read infile in
  Util.parse
    (object
       inherit Matcher.t s
       inherit Util.Lexers.ident ["read"; "write"; "skip"] s
       inherit Util.Lexers.decimal s
       inherit Util.Lexers.skip [
	 Matcher.Skip.whitespaces " \t\n";
	 Matcher.Skip.lineComment "--";
	 Matcher.Skip. nestedComment "(*" "*)"
       ] s
     end
    )
    (ostap (stmt -EOF))
