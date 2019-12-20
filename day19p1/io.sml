structure StdIO = struct
  type elem = int
  val reader =
    let val reader' = Option.valOf o TextIO.scanStream (Int.scan StringCvt.DEC)
    in fn () => reader' TextIO.stdIn
    end
  val writer = fn i => TextIO.print ((Int.toString i) ^ "\n")
end
