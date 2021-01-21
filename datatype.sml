datatype mytype = 
  TwoInts of int * int
  | Str of string
  | Pizza

val a = Str "hi"
val b = Pizza
val c = TwoInts (1 + 2, 3)
val d = Str

fun f (x : mytype) = 
  case x of
    Pizza => 3
    | Str s => String.size s
    | TwoInts (i1, i2) => i1 + i2

datatype exp = 
  Constant of int
  | Negate of exp
  | Add of exp * exp
  | Multiply of exp * exp

(* functions over recursive datatypes are usually recursive *)

fun eval e = 
   case e of 
    Constant i => i
    | Negate e2 => ~ (eval e2)
    | Add (e1, e2) => (eval e1) + (eval e2)
    | Multiply (e1, e2) => (eval e1) * (eval e2)

fun new_eval (Constant i) = i
  | new_eval (Negate e2) = ~ (new_eval e2)
  | new_eval (Add (e1, e2)) = (new_eval e1) + (new_eval e2)
  | new_eval (Multiply (e1, e2)) = (new_eval e1) * (new_eval e2)

fun number_of_adds e = 
  case e of
    Constant i => 0
    | Negate e2 => number_of_adds e2
    | Add (e1, e2) => 1 + number_of_adds e1 + number_of_adds e2
    | Multiply (e1, e2) => number_of_adds e1 + number_of_adds e2

fun max_constant e =
  let fun max_of_two (e1, e2) =
    let val m1 = max_constant e1
        val m2 = max_constant e2
    in if m1 > m2 then m1 else m2 end
  in
    case e of
      Constant i => i
      | Negate e2 => max_constant e2
      | Add (e1, e2) => max_of_two (e1, e2)
      | Multiply (e1, e2) => max_of_two (e1, e2)
  end

fun max_constant1 e = 
  let fun max_of_two (e1, e2) =
    Int.max (max_constant1 e1, max_constant1 e2)
  in case e of
    Constant i => i
    | Negate e2 => max_constant1 e2
    | Add (e1, e2) => max_of_two (e1, e2)
    | Multiply (e1, e2) => max_of_two (e1, e2)
  end

fun max_constant2 e =
  case e of
    Constant i => i
    | Negate e2 => max_constant1 e2
    | Add (e1, e2) => Int.max (max_constant1 e1, max_constant1 e2)
    | Multiply (e1, e2) => Int.max (max_constant1 e1, max_constant1 e2)

val example_exp : exp = Add (Constant (10 + 9), Negate (Constant 4))

val example_ans : int = eval example_exp

val example_addcount = number_of_adds (Multiply (example_exp, example_exp))

val nineteen = max_constant example_exp

(* lists and options are datatypes *)

datatype my_int_list = Empty | Cons of int * my_int_list

val xlist = Cons (4, Cons (23, Cons (12, Empty)))

fun append_my_list (xs, ys) = 
  case xs of
    Empty => ys
    | Cons (x, xs') => Cons (x, append_my_list (xs', ys))

(* try to use pattern-matching instead of `isSome` and `valOf` *)

fun inc_or_zero intoption = 
  case intoption of
    NONE => 0
    | SOME i => i + 1

fun sum_list xs = 
  case xs of
    [] => 0
    | x :: xs' => x + sum_list xs'

fun append (xs, ys) =
  case xs of
    [] => ys
    | x :: xs' => x :: append (xs', ys)

(* Polymorphic Datatypes *)

datatype 'a option = NONE | SOME of 'a

datatype 'a mylist = Empty | Cons of 'a * 'a mylist

datatype ('a, 'b) tree = 
  Node of 'a * ('a, 'b) tree * ('a, 'b) tree
  | Leaf of 'b

(* type is (int, int) tree -> int *)
fun sum_tree tr = 
  case tr of
    Leaf i => i
    | Node (i, lft, rgt) => i + sum_tree lft + sum_tree rgt

(* type is ('a, int) tree -> int *)
fun sum_leaves tr =
  case tr of
    Leaf i => i
    | Node (i, lft, rgt) => sum_leaves lft + sum_leaves rgt

(* type is ('a, 'b) tree -> int *)
fun num_leaves tr =
  case tr of
    Leaf i => 1
    | Node (i, lft, rgt) => num_leaves lft + num_leaves rgt
