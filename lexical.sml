(* x maps to 1 *)
val x = 1
(* f maps to a function that adds 1 to its argument *)
fun f y = x + y
(* x maps to 2 *)
val x = 2
(* y maps to 3 *)
val y = 3
(* call the function defined on line 4 with 5 *)
val z = f (x + y) (* z maps to 6 *)

fun filter (f, xs) =
  case xs of
    [] => []
    | x :: xs' => if f x
                  then x :: filter (f, xs')
                  else filter (f, xs')

fun allShorterThan1 (xs, s) = 
  filter (fn x => String.size x < String.size s, xs)

fun allShorterThan2 (xs, s) =
  let 
    val i = String.size s
  in 
    filter (fn x => String.size x < i, xs) 
  end
