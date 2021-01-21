datatype suit = Club | Diamond | Heart | Spade

datatype rank = Jack | Queen | King | Ace | Num of int

type card = suit * rank

fun is_Queen_of_Spades (c : card) =
  #1 c = Spade andalso #2 c = Queen

fun is_Queen_of_Spades2 c =
  case c of
    (Spade, Queen) => true
    | _ => false

val c1 : card = (Diamond, Ace)
val c2 : suit * rank = (Heart, Ace)
val c3 = (Spade, Ace)