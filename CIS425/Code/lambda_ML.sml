exception Error;

type var = string

datatype M = Var of var | Lam of var  * M | App of M * M

val ex_0 : M = Lam("x", Lam ("y", Var "x"))

(* Question 1: Write a function
     - string_of_term : term -> string
   This function traverses a term an turns it into its equivalent string representation
   That is :
     - string_of_term (Lam ("x", Lam ("y", Var "x"))) = "(\x. (\y. x))" *)

(************ Solution ***************)

fun  string_of_term (Var x)        = x
   | string_of_term (Lam (x, t))   = "(\\" ^ x ^ ". " ^ (string_of_term t) ^ ")"
   | string_of_term (App (t1, t2)) = "(" ^ (string_of_term t1) ^ " " ^ (string_of_term t2) ^ ")"


(*   function that prints a lambda term *) 

fun p_term x = print (string_of_term x ^ "\n")


(**********************************************************)
(*  We represents a set as a list.   We define the following 
    function:

     uniq  : given a list l, it returns l without duplicates 
     union : given two lists l1 and l2, join li and l2 together without having duplicates
     difference : given two lists l1 and l2, it removes l2 from l1
     remove : given a list l  and an element x, it removes x from l

 *)

fun foldl (f: 'a*'b->'b) (acc: 'b) (l: 'a list): 'b =
  case l of
    [] => acc
  | x::xs => foldl f (f(x,acc)) xs


fun exists [] n = false
|   exists (x :: xs) n = if n = x then true else exists xs n

fun rev nil = nil
|   rev (x::xs) = rev xs @ [x]

fun uniq  l = List.rev (foldl (fn (n, l1) => if (exists l1 n)
                                                      then l1
                                                      else n::l1
                                                     )
                        []
                        l)

fun union l1 l2 = uniq (l1@l2)

fun difference l1 l2 = foldl (fn (hd, acc) => if (exists l2 hd)
                                                   then acc else hd::acc)
                       [] l1

fun remove l1 x = foldl (fn (hd, acc) => if hd = x then acc else hd::acc) [] l1

(**********************************************************)

(*  free_variables returns the set of free variables of a lambda term .
    We represents a set as a list/ 

              free_variables : term -> var list

  For example:

  ` free_variables (Lambda ("x", Lambda ("y", Var "z"))) = ["z"] `
  ` free_variables (Lambda ("x", Lambda ("y", App (Var "x", Var "y")))) = [] `
  ` free_variables (Lambda ("x", App (Lambda ("y", Var "y"), Var "y")))` = ["y"]
*)

(*************************** Solution ***********************************)
fun free_variables (Var x)        = [x]
  | free_variables (Lam (x, t))   = remove (free_variables t) x
  | free_variables (App (t1, t2)) = union (free_variables t1) (free_variables t2)

(***********************************************************************)


(* Write a function **bound_variables** to calculate the set of bound variables of a given term *)




fun incr counter = counter := (!counter + 1);

val fresh  =
  let val counter = ref 0 in
  (fn () =>
    (incr counter; "$x" ^ Int.toString (!counter))
  )
  end


(* [new_name/var] M *)
fun rename x new_name (Var y)   = if  x = y then Var new_name else (Var y)
 |  rename x new_name (Lam (w,M)) = if x = w then Lam (w,M)
                                       else
                                       if w = new_name then raise Error
                                       else  Lam (w, rename x new_name M )
 |  rename x new_name (App (M, N)) = App (rename x new_name M,
                                             rename x new_name N)

(* fun not_free w N = not (exists (fn x => x = w) (free_variables N)) *)
fun not_free w N = not (exists (free_variables N) w) 

fun substitute N x (Var y )  = if x = y then N else (Var y)
|   substitute N x (Lam (w, M))  = if x = w then (Lam (w, M))
                              else
                   if  not_free w N then (Lam (w, substitute N x M))
                    else  let val new_x = fresh ()
                               in  substitute N x (Lam(new_x, rename w new_x M))
                          end
|   substitute N x (App(M, P)) = App(substitute N x M, substitute N x P)


(*******************************************************************************)


(* We are now ready to do beta reduction! *)

fun beta t = case t of
           App(Lam(x, M), N) => substitute N  x  M
          |t1                  => t1

fun alpha t =  case t of
              Lam(x,M) =>  let val new_name = fresh()
                            in  Lam(new_name, rename x new_name M)
                           end
             |    t  => t

(*******************************************************************************)

val t1 = App (Lam ("y", Var "y"), Lam ("x", Lam ("z", Var "z")))
val t2 = App(Lam( "x", App(Var "x", Lam("y", App(Var "y", Var "x")))), Var "y")

fun perform t = (print ("The term is  "); p_term t;
                 print ("The result of beta-reduction is ") ;
                 p_term (beta t));


