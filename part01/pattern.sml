exception ListLengthMismatch

fun zip list_triple =
  case list_triple of
    ([], [], []) => []
    | (hd1 :: tl1, hd2 :: tl2, hd3 :: tl3) => (hd1, hd2, hd3) :: zip (tl1, tl2, tl3)
    | _ => raise ListLengthMismatch

fun unzip lst = 
  case lst of
    [] => ([], [], [])
    | (a, b, c) :: tl => 
      let val (l1, l2, l3) = unzip tl
      in (a :: l1, b :: l2, c :: l3) end

fun nondecreasing xs = (* int list -> bool *)
  case xs of
    [] => true
    | x :: [] => true
    | head :: (neck :: rest) => head <= neck 
                                andalso nondecreasing (neck :: rest)

datatype sgn = P | N | Z

fun multsign (x1, x2) = (* int * int -> sgn *)
  let fun sign x = if x = 0 then Z else if x > 0 then P else N
  in
    case (sign x1, sign x2) of
      (Z, _) => Z
    | (_, Z) => Z
    | (P, P) => P
    | (N, N) => P
    | (N, P) => N
    | (P, N) => N
  end

fun append ([], ys) = ys
  | append (x :: xs', ys) = x :: append (xs', ys)

fun hd xs = 
  case xs of
    [] => raise List.Empty
  | x :: _ => x

fun maxlist (xs, ex) = (* int list * exn -> int *)
  case xs of
    [] => raise ex
  | x :: [] => x
  | x :: xs' => Int.max(x, maxlist (xs', ex))

exception MyException of int * int
exception MySimpleException

val x = maxlist ([3, 4, 5], MySimpleException)
        handle MySimpleException => 0

val w = maxlist ([], MyException (2, 3))
        handle MyException (x, y) => x + y
