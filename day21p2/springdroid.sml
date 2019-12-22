structure SpringDroid = struct
  datatype readable = A
                    | B
                    | C
                    | D
                    | E
                    | F
                    | G
                    | H
                    | I
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
         | E => #"E"
         | F => #"F"
         | G => #"G"
         | H => #"H"
         | I => #"I"
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
      (app (map Char.ord (explode "RUN\n")))
      o List.concat
      o (map (fn i => (encodeInst i) @ [Char.ord #"\n"]))
    end

  val printAscii : int -> unit = print o String.str o Char.chr

  fun run (proc : Intcode.process) (s : script) : int option =
    let
      val enc = encodeScript s
      fun printOut p =
        let
          val ran = Intcode.run p
        in
          if not (Intcode.isOut ran) then ran
          else
            let
              val (out, _, _) = ran
              val (outC, cont) = Intcode.getOut out
              val _ = printAscii outC
            in
              printOut cont
            end
        end
      fun inputScript (p : Intcode.result) s =
        foldl
        (fn (i, p) =>
          (printAscii i;
          Intcode.appIn (#1 (Intcode.run p)) i))
        (Intcode.appIn (#1 p) (printAscii (hd s); hd s))
        (tl s)
      val wPrompted = printOut proc
      val wInput = inputScript wPrompted enc
      fun printUntilDone p =
        let val ran = Intcode.run p
        in
          if not (Intcode.isOut ran) then NONE
          else
            let
              val (out, _, _) = ran
              val (outC, cont) = Intcode.getOut out
            in
              if outC > 255
              then SOME outC
              else (printAscii outC; printUntilDone cont)
            end
        end
    in
      printUntilDone wInput
    end
end
