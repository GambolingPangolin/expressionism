nil = { C_0^0 }
cons x xs = { C_1^2 x xs }

sum xs = {
  case xs of
    C_0 -> 0 ;
    C_1 h t -> + h (sum t)
}

take n xs = {
  let next = case xs of
        C_0 -> nil;
        C_1 h t -> take (- n 1) t
  in if (> n 0) next xs
}
