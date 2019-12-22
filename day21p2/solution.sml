structure Solution = struct
  fun solution p =
    let
      open SpringDroid
      val script = [
      NOT(A,WJ),
      NOT(B,WT),
      OR(RT,WJ),
      NOT(C,WT),
      OR(RT,WJ),
      AND(D,WJ)
                   ]
    in
      run p script
    end
  val solve = solution o Intcode.load o Reader.readFromFile
end
