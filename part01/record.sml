type name_record = {
  student_num : int option,
  first : string,
  middle : string option,
  last : string
}

fun sum_triple triple = 
  case triple of
    (x, y, z) => x + y + z

fun sum_triple1 triple = 
  let val (x, y, z) = triple
  in
    x + y + z
  end

fun sum_triple2 (x, y, z) = x + y + z

fun full_name r =
  case r of
    {first = x, middle = y, last = z} => x ^ " " ^ " " ^ z
