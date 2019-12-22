structure Solution = struct
  fun solution p =
    let
      open SpringDroid
      val script = [NOT (D, WJ)] (* dummy program to get started *)
    in
      run p script
    end
  val solve = solution o Intcode.load o Reader.readFromFile
end
