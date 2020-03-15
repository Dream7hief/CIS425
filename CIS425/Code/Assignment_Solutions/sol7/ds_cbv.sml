Control.Print.printDepth:= 100;
use "parser.sml";
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
  | RES_FUN   of (string * term);

exception UnboundID of string
datatype env = Env of string -> result
fun emptyenvFun  (x : string) : result = raise (UnboundID x);
val emptyenv = Env emptyenvFun
fun update (Env e) (x : string) (ty : result) y = if x = y then ty else e y

fun interp (Env e, AST_ID i)          = e i
  | interp (env, AST_NUM n)         = RES_NUM n
  | interp (env, AST_BOOL b)        = RES_BOOL b
  | interp (env, AST_FUN (i,e))     = RES_FUN (i,e)
  | interp (env, AST_APP (e1,e2))   =
    (case interp (env, e1) of
         RES_FUN (v, body) =>
           interp (Env (update env v (interp (env,e2))),body)
       | RES_SUCC =>
         (case interp (env,e2) of
              RES_NUM n => RES_NUM (n+1)
            | _ => RES_ERROR "succ non-number")
       | RES_PRED =>
         (case interp (env,e2) of
              RES_NUM n => RES_NUM (n+1)
            | _ => RES_ERROR "pred non-number")
       | RES_ISZERO =>
         (case interp (env,e2) of
              RES_NUM n => RES_BOOL (n = 0)
            | _ => RES_ERROR "iszero non-number")
       | _ => RES_ERROR "apply non-function")
  | interp (env, AST_SUCC)          = RES_SUCC
  | interp (env, AST_PRED)          = RES_PRED
  | interp (env, AST_ISZERO)        = RES_ISZERO
  | interp (env, AST_IF (e1,e2,e3)) =
    case interp (env,e1) of
        RES_BOOL true =>
          interp (env,e2)
      | RES_BOOL false =>
          interp (env,e3)
      | _              => RES_ERROR "if condition non-bool!"


(* (fn x => 42) ((fn x => x x) (fn x => x x)) *)
(* val test1 =
    AST_APP
        (AST_FUN ("x",AST_NUM 42)
        ,AST_APP
             (AST_FUN ("x",AST_APP(AST_ID "x",AST_ID "x"))
             ,AST_FUN ("x",AST_APP(AST_ID "x",AST_ID "x")))) *)

(* ((fn x => fn y => x 42) (fn y => y)) 0 *)
val test2 =
    AST_APP
        (AST_APP
             (AST_FUN ("x",AST_FUN ("y",AST_APP (AST_ID "x"
                                                ,AST_NUM 42)))
             ,AST_FUN ("y",AST_ID "y"))
        ,AST_NUM 0)

val test3 =
    AST_APP
        (AST_FUN ("x",AST_FUN ("y",AST_APP (AST_ID "x"
                                           ,AST_NUM 42)))
        ,AST_FUN ("y",AST_ID "y"))

val test5 =
    AST_APP
        (AST_APP
             (AST_FUN ("x",AST_FUN ("y",AST_ID "x"))
             ,AST_NUM 0)
        ,AST_NUM 1)

val test6 =
    AST_APP
        (AST_APP
             (AST_FUN ("x",AST_FUN ("y",AST_ID "y"))
             ,AST_NUM 0)
        ,AST_NUM 1)

val test7 =
    AST_APP (AST_FUN ("x",AST_ID "x"),AST_BOOL true)

(* (fn x => (fn f => (fn x => f 0) 42) (fn y => x)) 5 *)
val test8 =
    AST_APP
        (AST_FUN ("x"
                 ,AST_APP
                      (AST_FUN ("f"
                               ,AST_APP
                                    (AST_FUN ("x",AST_APP
                                                      (AST_ID "f"
                                                      ,AST_NUM 0))
                                    ,AST_NUM 42))
                      ,AST_FUN ("y",AST_ID "x")))

        ,AST_NUM 5)


val eval = fn s => interp (emptyenv, parsestr s)