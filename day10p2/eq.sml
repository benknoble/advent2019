structure Eq = struct
  fun classes (r : 'a -> 'a -> bool) (xs : 'a list) : 'a list list =
    let
      fun classes' cls x =
        if null cls then [[x]]
        else
          let
            val cls' = map (fn cl => case cl of
                                          [] => [x]
                                        | hd::_ => if r x hd then x::cl else cl) cls
            val matches_any = List.exists (fn cl => case cl of
                                                         [] => false
                                                       | hd::_ => r x hd) cls
          in
            if matches_any then cls'
            else [x]::cls
          end
    in
      foldl (fn (x, cls) => classes' cls x) [] xs
    end
end
