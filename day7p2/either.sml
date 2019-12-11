(* a right-biased either type, based on scala.util.Either *)
structure Either = struct
  datatype ('a, 'b) either = L of 'a
                           | R of 'b
  fun fold (l : 'a -> 'c) (r : 'b -> 'c) (e : ('a, 'b) either) : 'c =
    case e of
         L a => l a
       | R b => r b
  fun map (f : 'b -> 'c) : ('a, 'b) either -> ('a, 'c) either =
    fold (fn a => L a) (fn b => R (f b))
  fun flatMap (f : 'b -> ('a, 'b) either) : ('a, 'b) either -> ('a, 'b) either =
    fold (fn a => L a) (fn b => f b)
end
