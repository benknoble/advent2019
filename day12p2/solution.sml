structure Solution = struct
  val readFile = TextIO.inputAll o TextIO.openIn
  val nbody = NBody.fromString o readFile
  fun solve s =
    let val nbody = nbody s
    in NBody.findPeriod nbody
    end
end
