structure Solution = struct
  val readFile = TextIO.inputAll o TextIO.openIn
  val nbody = NBody.fromString o readFile
  fun solve s =
    let val nbody = nbody s
    in NBody.tot (NBody.stepN 1000 nbody)
    end
end
