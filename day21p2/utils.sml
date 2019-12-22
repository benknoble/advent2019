structure Utils = struct
  fun digits (i : int) : int list =
  let
    fun digits' i' acc =
      case i' of
            0 => acc
          | i'' => digits' (i'' div 10) ((i'' mod 10)::acc)
  in
    digits' i []
  end

  val fromDigits = List.foldl (fn (d, i) => i*10 + d) 0

  fun pad (n : int) (z : 'a) (xs : 'a list) : 'a list =
    let val length = length xs
    in
      if length >= n then xs
      else (List.tabulate (n - length,(fn i => z))) @ xs
    end

  fun padRight n z xs = rev (pad n z (rev xs))
end
