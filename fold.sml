fun fold (f, acc, xs) = 
  case xs of 
    [] => acc
    | x :: xs' => fold (f, f (acc, x), xs')

fun f1 xs = fold ((fn (x, y) => x + y), 0, xs)

fun f2 xs = fold ((fn (x, y) => x andalso y >= 0), true, xs)

fun f3 (xs, lo, hi) = 
  fold ((fn (x, y) => x + (if y >= lo andalso y <= hi then 1 else 0)), 0, xs)

fun f4 (xs, s) = 
  let val i = String.size s
  in fold ((fn (x, y) => x andalso String.size y < i), true, xs) end

fun f5 (g, xs) = fold ((fn (x, y) => x andalso g y), true, xs)
