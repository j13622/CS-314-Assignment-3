open List
open Ast
open ExpressionLibrary

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec insert tree x =
  match tree with
  | Leaf -> Node(Leaf, x, Leaf)
  | Node(l, y, r) ->
     if x = y then tree
     else if x < y then Node(insert l x, y, r)
     else Node(l, y, insert r x)

let construct l =
  List.fold_left (fun acc x -> insert acc x) Leaf l

let rec print_float_list lst =   match lst with   | [] -> ()   | [x] -> print_float x; print_newline ()   | x :: xs -> print_float x; print_string "; "; print_float_list xs  

(**********************************)
(* Problem 1: Tree In-order Fold  *)
(**********************************)

let rec inorder t =
match t with
 | Leaf -> []
 | Node(l, y, r) -> (inorder l) @ [y] @ (inorder r)

let rec fold_inorder f acc t =
  let lst = inorder t in
  List.fold_left f acc lst

(**********************************)
(* Problem 2: BST Remove *)
(**********************************)

let rec remove x t =
  match t with
  | Leaf -> Leaf
  | Node(l, y, r) ->
    if y = x then
      match t with
      | Leaf -> Leaf
      | Node(Leaf, _, Leaf) -> Leaf
      | Node(l, _, Leaf) -> l
      | Node(Leaf, _, r) -> r
      | Node(l, _, r) -> 
        let succ =  fold_inorder (fun acc x -> acc @ [x]) [] r in
        match succ with
          | [] -> Leaf
          | h::_ -> Node(l, h, remove h r)
    else if x > y then Node(l, y, remove x r)
    else Node(remove x l, y, r)


(* ------ Type definitions for the abstract syntax tree defined in ast.ml ------- *)

(**********************************
    type binop = Add | Sub | Mul

    type expression =
      | Num of float
      | Var
      | Binop of binop * expression * expression
***********************************)



(**********************************
    There are some functions from expressionLibrary that you can use to debug your code.

    `parse: string -> expression` :
        translates a string in infix form (such as `x*x - 3.0*x + 2.5`) into an expression
        (treating `x` as the variable). The parse function parses according to the standard
        order of operations - so `5+x*8` will be read as `5+(x*8)`.
    `to_string: expression -> string` :
        prints expressions in a readable form, using infix notation. This function adds
        parentheses around every binary operation so that the output is completely unambiguous.
    `to_string_wo_paren: expression -> string` :
        prints expressions in a readable form, using infix notation. This function does not
        add any parentheses so it can only be used for expressions in standard forms.
    `make_exp: int -> expression` :
        takes in a length `l` and returns a randomly generated expression of length at most `2l`.
    `rand_exp_str: int -> string` :
        takes in a length `l` and returns a string representation of length at most `2l`.

    For example,

    let _ =
      (* The following code make an expression from a string
         "5*x*x*x + 4*x*x + 3*x + 2 + 1", and then convert the
         expression back to a string, finally it prints out the
         converted string
         *)
      let e = parse ("5*x*x*x + 4*x*x + 3*x + 2 + 1") in
      let s = to_string e in
      print_string (s^"\n")

    let _ =
      (* The following code make a random expression from a string
         and then convert the expression back to a string
         finally it prints out the converted string
         *)
      let e = make_exp 10 in
      let s = to_string e in
      print_string (s^"\n")
***********************************)




(**********************************)
(* Problem 3: Evaluation  *)
(**********************************)

(* evaluate : evaluates an expression for a particular value of x.
*  Example : evaluate (parse "x*x + 3") 2.0 = 7.0 *)
let rec evaluate (e:expression) (x:float) : float =
  match e with
  | Var -> x
  | Num(t1) -> t1
  | Binop(b, t1, t2) ->
    let v1 = evaluate t1 x in
    let v2 = evaluate t2 x in
    match b with
    | Add -> v1 +. v2
    | Sub -> v1 -. v2
    | Mul -> v1 *. v2




(**********************************)
(* Problem 4: Derivatives  *)
(**********************************)

let rec derivative (e:expression) : expression =
  match e with
  | Num(_) -> Num(0.0)
  | Var -> Num(1.0)
  | Binop(b, t1, t2) ->
    let v1 = derivative t1 in
    let v2 = derivative t2 in
    if b = Mul then Binop(Add, Binop(Mul, v1, t2), Binop(Mul, v2, t1)) else Binop(b, v1, v2)


(**********************************)
(* Problem 5: Find Zero  *)
(**********************************)

let rec find_zero_helper (e1:expression) (e2:expression) (xn:float) (epsilon:float) (lim:int) : float option = 
  if lim = 0 then None else
  let v1 = evaluate e1 xn in
  if (abs_float v1) < epsilon then Some(xn) else
  let v2 = evaluate e2 xn in
  let xn1 = xn -. (v1/.v2) in
  find_zero_helper e1 e2 xn1 epsilon (lim-1)

let find_zero (e:expression) (xn:float) (epsilon:float) (lim:int)
  : float option =
  let e2 = derivative e in
  find_zero_helper e e2 xn epsilon lim

(**********************************)
(* Problem 6: Simplification  *)
(**********************************)

let rec factorial n =
    if n <= 1 then
      1
    else
      factorial (n - 1) * n

let rec clean (e:expression) : expression = 
  match e with
  | Num(a) -> Num(a)
  | Var -> Var
  | Binop(b, t1, t2) -> 
    let v1 = clean t1 in
    let v2 = clean t2 in
    match (b, v1, v2) with
    |(Mul, Num(x), Num(y)) -> Num(x*.y)
    |(Add, Num(x), Num(y)) -> Num(x+.y)
    |(Sub, Num(x), Num(y)) -> Num(x-.y)
    |(Mul, v1, v2) -> if (v1 = Num(0.0) || v2 = Num(0.0)) then Num(0.0) else Binop(Mul, v1, v2)
    |(Sub, v1, v2) -> if v1 = v2 then Num(0.0) else Binop(Sub, v1, v2)
    |(Add, v1, v2) -> Binop(Add, v1, v2)

let rec simplify_helper (e:expression) (l:float list) : float list = 
  let curr = evaluate e 0.0 in
  let x = clean (derivative e) in
  let m = curr::l in
  match x with
  | Num(a) -> a::m
  | Var -> simplify_helper x m
  | Binop(_, _, _) -> simplify_helper x m

let rec mult count = 
  if count = 1 then Var else if count = 2 then Binop(Mul, Var, Var) else 
  Binop(Mul, Var, mult (count-1))

let simplify (e:expression) : expression =
  let lst = simplify_helper e [] in
  let len = (List.length lst) - 1 in
  let mapper1 ind a = a/.(float_of_int (factorial (len - ind))) in
  let mid_lst = mapi mapper1 lst in
  let mapper2 ind a = if len = ind then Num(a) else if a = 0. then Num(0.) else Binop(Mul, Num(a), mult (len-ind)) in
  let fin_lst = mapi mapper2 mid_lst in
  let folder acc next = if acc = Num(0.) then next else if next = Num(0.) then acc else Binop(Add, acc, next) in
  List.fold_left folder (Num(0.)) (fin_lst)


(*****************************************)
(* Problem 7: Automatic Differentiation *)
(*****************************************)

(*

"Forward mode automatic differentiation", has become an
important algorithm (since 2017 or so) in deep learning.
You can read about it in section 3.1 of this paper:
http://jmlr.org/papers/volume18/17-468/17-468.pdf
"Automatic Differentiation in Machine Learning: A Survey"
(and pay particular attention to Table 2 for a worked example).

So, the challenge (which is actually not very difficult) is,
write this function

 let evaluate2 (e: expression) (x: float) : float * float = ...

that computes both e(x) and the first derivative e'(x),
without ever calculating (derivative e).  Like evaluate,
do it by case analysis on the syntax-tree of e.

*)

let rec evaluate2 (e: expression) (x: float) : float * float =
  match e with
  | Var -> (x, 1.)
  | Num(t1) -> (t1, 0.)
  | Binop(b, t1, t2) ->
    let (v1, dv1) = evaluate2 t1 x in
    let (v2, dv2) = evaluate2 t2 x in
    match b with
    | Add -> (v1 +. v2, dv1 +. dv2)
    | Sub -> (v1 -. v2, dv1 -. dv2)
    | Mul -> (v1 *. v2, v1*.dv2 +. v2*.dv1)

(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in
  let bonus_count = ref 1 in

 (* Testcases for fold_inorder *)
  let _ =
    try
      assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = [1;2;3]);
      assert (fold_inorder (fun acc x -> acc + x) 0 (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = 6)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

 (* Testcases for remove *)
  let _ =
    try
      assert (remove 20 (Node (Node (Node (Leaf, 20, Leaf), 30, Node (Leaf, 40, Leaf)), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf))))
              = (Node (Node (Leaf,                  30, Node (Leaf, 40, Leaf)), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf)))));
      assert (remove 30 (Node (Node (Leaf,                  30, Node (Leaf, 40, Leaf)), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf))))
              = (Node (Node (Leaf,                  40, Leaf                 ), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf)))));
      assert (remove 50 (Node (Node (Leaf,                  40, Leaf                 ), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf))))
              = (Node (Node (Leaf,                  40, Leaf                 ), 60, Node (Leaf,                  70, Node (Leaf, 80, Leaf)))))
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

 (* Testcases for evaluate *)
  let _ =
    try
      assert (evaluate (parse "x*x + 3.0") 2.0 = 7.0)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

 (* Testcases for derivative *)
  let _ =
    try
      assert (evaluate (derivative (parse "x*x + 3.0")) 2.0 = 4.0);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

 (* Testcases for zero finding *)
  let _ =
    try
      let e = (parse "2*x*x - x*x*x - 2") in
      let g, epsilon, lim = 3.0, 1e-3, 50 in
      let x = find_zero e g epsilon lim in
      match x with
      | None -> assert false
      | Some x ->
          let eval_result = evaluate e x in
          assert (0. -. epsilon < eval_result && eval_result < epsilon)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

 (* Testcases for simplify *)
  let _ =
    try
      (*print_string (to_string_wo_paren (simplify (parse "3*x*x + 8*x + 2*x*x - 5 - 5*x")));
       print_string (to_string_wo_paren (simplify (parse "(x-1)*x*(x-5)")));
       print_string (to_string_wo_paren (simplify (parse "x - x")));
      print_string (to_string_wo_paren (simplify (parse "x + x + 0")));
      print_string (to_string_wo_paren (simplify (parse "(x-1)*x*(x-5)*(4*x*x*x-11+66*x)")));*)
      assert (to_string_wo_paren (simplify (parse "3*x*x + 2*x - 5 + 4*x*x - 7*x")) = "7.*x*x+-5.*x+-5.");
      assert (to_string_wo_paren (simplify (parse "3*x*x + 8*x + 2*x*x - 5 - 13*x")) = "5.*x*x+-5.*x+-5.");
      assert (to_string_wo_paren (simplify (parse "(x-1)*x*(x-5)")) = "1.*x*x*x+-6.*x*x+5.*x");
      assert (to_string_wo_paren (simplify (parse "(x-1)*x*(x-5)*(4*x*x*x-11+66*x)")) = "4.*x*x*x*x*x*x+-24.*x*x*x*x*x+86.*x*x*x*x+-407.*x*x*x+396.*x*x+-55.*x");
      assert (to_string_wo_paren (simplify (parse "x - x")) = "0.");
      assert (to_string_wo_paren (simplify (parse "x + x + 0")) = "2.*x+0.");
      assert (to_string_wo_paren (simplify (parse "0")) = "0.")
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in
 (* Testcases for evaluate2 *)
  let _ =
    try
      assert (evaluate2 (parse "x*x + 3") 2.0 = (7.0, 4.0))
    with e -> (bonus_count := !bonus_count - 1; print_string ((Printexc.to_string e)^"\n")) in

  if !error_count = 0 then Printf.printf ("Passed all testcases.\n")
  else Printf.printf ("%d out of 6 programming questions are incorrect.\n") (!error_count);

  if !bonus_count = 0 then Printf.printf ("The bonus problem is not solved.\n")
  else Printf.printf ("The bonus problem is solved.\n")

let _ = main()
