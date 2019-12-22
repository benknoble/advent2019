structure Solution = struct
  fun solution p =
    let
      open SpringDroid
      val script = [
      NOT(H,WT),
      OR(C,WT),
      AND(B,WT),
      AND(A,WT),
      NOT(RT,WJ),
      AND(D,WJ)
                   ]
    in
      run p script
    end
  val solve = solution o Intcode.load o Reader.readFromFile
end
