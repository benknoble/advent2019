structure Decoder = IntDecoderFn (Memory)
structure Intcode = CPUFn (structure Memory = Memory
                           structure Decoder = Decoder)

structure Reader = struct
  fun read s : Intcode.program =
  let
    val collect = List.mapPartial Int.fromString
    fun sep c = Char.isSpace c orelse c = #","
    val tokenize = String.tokens sep
  in
    (Memory.base, (collect o tokenize) s)
  end
  val readFromStream : TextIO.instream -> Intcode.program =
    read o TextIO.inputAll
  val readFromFile : string -> Intcode.program =
    readFromStream o TextIO.openIn
end

(* useful interactively *)
structure I = struct
  fun interpret s = Intcode.interpret (Reader.read s) StdIO.reader StdIO.writer
end
