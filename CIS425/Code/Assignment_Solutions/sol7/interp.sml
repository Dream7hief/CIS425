(*  Here's a skeleton file to help you get started on Interpreter 1.
 * Original version by Geoffrey Smith - http://users.cs.fiu.edu/~smithg/
 *)

datatype term
  = AST_ID of string   (* x *)
  | AST_NUM of int
  | AST_BOOL of bool
  | AST_FUN of (string * term) (* fn x => 1 *)
  | AST_APP of (term * term)
  | AST_SUCC
  | AST_PRED
  | AST_ISZERO
  | AST_IF of (term * term * term)


datatype result
  = RES_ERROR of string
  | RES_NUM   of int
  | RES_BOOL  of bool
  | RES_SUCC
  | RES_PRED
  | RES_ISZERO
  | RES_FUN   of (string * term)
  | RES_CLOSURE of (string * term * env)
and env = Env of (string -> result)

(*  An environment is a function string -> result.
 *  I've included code for the empty environment
 *  and code that produces the updated environment E[x : t].
 *)

exception UnboundID

fun emptyenvFun  (x : string) : result = raise UnboundID
val emptyenv = Env emptyenvFun

(*  update : (string -> result) -> string -> result -> string -> result  *)
             (* env : *)
fun update (Env e) (x : string) (ty : result) y = if x = y then ty else e y

fun interp (Env env, AST_ID i)          = env i 
  | interp (env, AST_NUM n)             = RES_NUM n 
  | interp (env, AST_BOOL b)            = RES_BOOL b 
  | interp (env, AST_FUN (i,e))         = RES_CLOSURE (i, e, env)
  | interp (env, AST_APP (e1,e2))       = (case interp (env, e1) of 
                                             RES_CLOSURE (x, t, env) => (case interp (env, e2) of 
                                                                         ans => (interp (Env (update env x ans), t)))
                                            |a                       => RES_ERROR "expected a function")
              
  | interp (env, AST_SUCC)          = RES_SUCC
  | interp (env, AST_PRED)          = RES_PRED
  | interp (env, AST_ISZERO)        = RES_ISZERO
  | interp (env, AST_IF (e1,e2,e3)) = case interp (env, e1) of 
                 RES_BOOL b => (if b then interp (env, e2) else interp (env, e3))
                | _          => RES_ERROR "expected a bool"


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
