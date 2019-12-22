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
    let
      val enc = encodeScript s
      fun printPrompt p =
        let
          val ran = Intcode.run p
        in
          if not (Intcode.isOut ran) then ran
          else
            let
              val (outPrompt, _, _) = ran
              val (prompt, cont) = Intcode.getOut outPrompt
              val _ = printAscii prompt
            in
              printPrompt cont
            end
        end
      fun inputScript (p : Intcode.result) s =
        foldl
        (fn (i, p) =>
          (printAscii i;
          Intcode.appIn (#1 (Intcode.run p)) i))
        (Intcode.appIn (#1 p) (printAscii (hd s); hd s))
        (tl s)
      val wPrompted = printPrompt proc
      val wInput = inputScript wPrompted enc
    in
      NONE
    end
end
