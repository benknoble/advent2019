structure SpringDroid = struct
  datatype readable = A
                    | B
                    | C
                    | D
                    | RJ
                    | RT
  datatype writable = WJ
                    | WT
  type params = readable * writable
  datatype inst = AND of params
                | OR of params
                | NOT of params
  type script = inst list

  fun encodeParams ((r, w) : params) : int list =
    map Char.ord
    [(case r of
           A => #"A"
         | B => #"B"
         | C => #"C"
         | D => #"D"
         | RJ => #"J"
         | RT => #"T"),
     #" ",
     (case w of
           WJ => #"J"
         | WT => #"T")]

  fun encodeInst (i : inst) : int list =
    let fun encode' s p = (map Char.ord (explode s)) @ (encodeParams p)
    in case i of
            AND p => encode' "AND " p
          | NOT p => encode' "NOT " p
          | OR p => encode' "OR " p
    end

  val encodeScript : script -> int list =
    let val app = fn a => fn b => b @ a
    in
      (app (map Char.ord (explode "WALK\n")))
      o List.concat
      o (map (fn i => (encodeInst i) @ [Char.ord #"\n"]))
    end

  val printAscii : int -> unit = print o String.str o Char.chr

  fun run (proc : Intcode.process) (s : script) : int option =
      NONE
end
