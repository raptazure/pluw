fun silly1 (z : int) = 
  let 
    val x = if z > 0 then z else 34
    val y = x + z + 9
  in 
    if x > y then x * 2 else y * y
  end

fun silly2 () =
  let 
    val x = 1
  in 
    (let val x = 2 in x + 1 end) + (let val y = x + 2 in y + 1 end)
  end

fun countup_from (x : int) =
  let 
    fun count (from : int) =
      if from = x
      then x :: []
      else from :: count (from + 1)
  in
    count (1)
  end

fun max (xs : int list) =
  if null xs
  then 0
  else if null (tl xs)
  then hd xs
  else 
    (* for style, could also use a let-binding for (hd xs) *)
    let val tl_ans = max (tl xs)
    in 
      if hd xs > tl_ans
      then hd xs
      else tl_ans
    end
