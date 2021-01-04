fun double x = x * 2
fun incr x = x + 1
val a_tuple = (double, incr, double(incr 7))
val eighteen = (#1 a_tuple) 9

(* ('a -> 'a) * int * 'a -> 'a *)
fun n_times (f, n, x) = 
  if n = 0 
  then x
  else f (n_times (f, n - 1, x))

fun increment x = x + 1
fun double x = x + x

val x1 = n_times (double, 4, 7)
val x2 = n_times (increment, 4, 7)
val x3 = n_times (tl, 2, [4, 8, 12, 16])

fun triple_n_times (n, x) = 
  n_times (let fun triple x = x * 3 in triple end, n, x)

fun triple_n_times' (n, x) = 
  n_times ((fn x => 3 * x), n, x)

(* ('a -> 'b) * 'a list -> 'b list *)
fun map (f, xs) =
  case xs of
    [] => []
    | x :: xs' => (f x) :: map (f, xs')

val x1 = map ((fn x => x + 1), [4, 8, 12])
val x2 = map (hd, [[1, 2], [3, 4], [5, 6, 7]])

(* ('a -> bool) * 'a list -> 'a list *)
fun filter (f, xs) = 
  case xs of
    [] => []
    | x :: xs' => if f x
                  then x :: (filter (f, xs'))
                  else filter (f, xs')

fun is_even v = 
  (v mod 2 = 0)

fun all_even xs = filter (is_even, xs)

fun all_even_snd xs = filter ((fn (_, v) => is_even v), xs)

(* returning a function *)
fun double_or_tripple f =
  if f 7 
  then fn x => 2 * x
  else fn x => 3 * x

val double = double_or_tripple (fn x => x - 3 = 4)
val nine = (double_or_tripple (fn x => x = 42)) 3

datatype exp = Constant of int
              | Negagte of exp
              | Add of exp * exp
              | Multiply of exp * exp

(* higher-order predicate *)
fun true_of_all_constants (f, e) =
  case e of
    Constant i => f i
    | Negagte e1 => true_of_all_constants (f, e1)
    | Add (e1, e2) => true_of_all_constants (f, e1) 
                      andalso true_of_all_constants (f, e2)
    | Multiply (e1, e2) => true_of_all_constants (f, e1) 
                      andalso true_of_all_constants (f, e2)
    
fun all_even e = true_of_all_constants ((fn x => x mod 2 = 0), e)
