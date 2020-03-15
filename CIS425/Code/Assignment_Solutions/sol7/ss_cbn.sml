(*  Here's a skeleton file to help you get started on Interpreter 1.
 * Original version by Geoffrey Smith - http://users.cs.fiu.edu/~smithg/
 *)

use "parser.sml"; 

Control.Print.printDepth:= 100;

(* datatype term
  = AST_ID of string
  | AST_NUM of int
  | AST_BOOL of bool
  | AST_FUN of (string * term)
  | AST_APP of (term * term)
  | AST_SUCC
  | AST_PRED
  | AST_ISZERO
  | AST_IF of (term * term * term) *)

datatype result
  = RES_ERROR of string
  | RES_NUM   of int
  | RES_BOOL  of bool
  | RES_SUCC
  | RES_PRED
  | RES_ISZERO
  | RES_CLOS  of (string * term * env)
and env = Env of (string -> (env * term));


(*  An environment is a function string -> result.
 *  I've included code for the empty environment
 *  and code that produces the updated environment E[x : t].
 *)

exception UnboundID
fun emptyenvFun  (x : string) : (env * term) = raise UnboundID;
val emptyenv = Env emptyenvFun

(*  update : (string -> result) -> string -> result -> string -> result  *)
fun update (Env e) (x : string) (ty : (env * term)) y = if x = y then ty else e y

(*  Here's a partial skeleton of interp : (env * term) -> result.
    I've done the first case for you
*)
fun interp (Env e, AST_ID i)        = interp (e i)
  | interp (env, AST_NUM n)         = RES_NUM n
  | interp (env, AST_BOOL b)        = RES_BOOL b
  | interp (env, AST_FUN (i,e))     = RES_CLOS (i,e,env)
  | interp (env, AST_APP (e1,e2))   =
    (let val v1 = interp (env, e1)
    in
        (case v1 of
            RES_CLOS (x, y, e) =>
            interp (Env (update e x (env, e2)), y)
          | RES_SUCC           => (case interp (env, e2) of 
                RES_NUM n => RES_NUM (n + 1)
              | _         => RES_ERROR "succ argument not an integer!")
          | RES_PRED           => (case interp (env, e2) of 
                RES_NUM n => if n <= 0 then (RES_NUM 0) else RES_NUM (n - 1)
              | _         => RES_ERROR "pred argument not an integer!")
          | RES_ISZERO         => (case interp (env, e2) of
                RES_NUM n => RES_BOOL (n = 0)
              | _         => RES_ERROR "iszero argument not an integer!")
          | _             => RES_ERROR "can only apply functions!")
    end)
  | interp (env, AST_SUCC)          = RES_SUCC
  | interp (env, AST_PRED)          = RES_PRED
  | interp (env, AST_ISZERO)        = RES_ISZERO
  | interp (env, AST_IF (e1,e2,e3)) =
    case interp (env,e1) of
        RES_BOOL true =>
          interp (env,e2)
      | RES_BOOL false =>
          interp (env,e3)
      | _              => RES_ERROR "if condition should evaluate to boolean!"

val omega = AST_APP
                (AST_FUN ("x",AST_APP(AST_ID "x",AST_ID "x"))
                ,AST_FUN ("x",AST_APP(AST_ID "x",AST_ID "x")))
val test1 = AST_APP
                (AST_FUN ("x",AST_NUM 42)
                ,omega)

(* ((fn x => fn y => x 42) (fn y => y)) 0 *)
val test2 =
    AST_APP
        (AST_APP
             (AST_FUN ("x",AST_FUN ("y", AST_APP (AST_ID "x",AST_NUM 42)))
             ,AST_FUN ("y",AST_ID "y"))
        ,AST_NUM 0)

val test3 =
    AST_APP
        (AST_FUN ("x",AST_ID "x")
        ,AST_NUM 42)



val eval = fn s => interp (emptyenv, parsestr s)
