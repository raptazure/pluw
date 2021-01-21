(* Combining functions *)

fun compose (f, g) = fn x => f (g x)

fun sqrt_of_abs i = Math.sqrt (Real.fromInt (abs i))

fun sqrt_of_abs' i = (Math.sqrt o Real.fromInt o abs) i

val sqrt_of_abs'' = Math.sqrt o Real.fromInt o abs

infix !>

fun x !> f = f x

fun sqrt_of_abs''' i = i !> abs !> Real.fromInt !> Math.sqrt

fun backup1 (f, g) = fn x => case f x of 
                                NONE => g x
                              | SOME y => y

fun backup2 (f, g) = fn x => f x handle _ => g x

(* Currying *)

val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x

val t1 = ((sorted3 7) 9) 11

val t2 = sorted3 7 9 11

fun sorted3_nicer x y z = z >= y andalso y >= x

fun fold f acc xs = 
  case xs of
    [] => acc
    | x :: xs' => fold f (f(acc, x)) xs'

fun sum xs = fold (fn (x, y) => x + y) 0 xs 

fun range i j = if i > j then [] else i :: range (i + 1) j

val countup = range 1

fun exists predicate xs =
  case xs of
    [] => false
  | x :: xs' => predicate x orelse exists predicate xs'

val hasZero = exists (fn x => x = 0)

(* Currying Wrapup *)

fun curry f = fn x => fn y => f (x, y)

fun curry1 f x y = f (x, y)

fun uncurry f (x, y) = f x y

datatype set = S of {
  insert : int -> set,
  member : int -> bool,
  size : unit -> int
}

val empty_set = 
  let 
    fun make_set xs =
      let 
        fun contains i = List.exists (fn j => i = j) xs
      in
        S { insert = fn i => if contains i 
              then make_set xs else make_set (i :: xs),
            member = contains,
            size = fn () => length xs
          }
      end
  in
    make_set []
  end

fun use_sets () =
  let val S s1 = empty_set
      val S s2 = (#insert s1) 34
      val S s3 = (#insert s2) 34
      val S s4 = #insert s3 19
  in
      if (#member s4) 42
      then 99
      else if (#member s4) 19
      then 17 + (#size s3) ()
      else 0
  end

datatype 'a mylist = Cons of 'a * ('a mylist) | Empty

fun map f xs =
  case xs of
    Empty => Empty
  | Cons (x, xs) => Cons (f x, map f xs)

fun filter f xs = 
  case xs of
    Empty => Empty
  | Cons (x, xs) => if f x
                    then Cons (x, filter f xs)
                    else filter f xs

fun length xs = 
  case xs of
    Empty => 0
  | Cons (x, xs) => 1 + length xs

val doubleAll = map (fn x => x * 2)

fun countNs (xs, n : int) = length (filter (fn x => x = n) xs)
