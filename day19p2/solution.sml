structure Solution = struct
  fun solution p = Tractor.countPullingRange p 0 50
  val solve = solution o Intcode.load o Reader.readFromFile
end
