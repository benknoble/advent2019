structure Solution = struct
  fun solution p =
    Option.map
      (Point.map (fn (x,y) => 10000 * x + y))
      (Tractor.closestToTractor (Tractor.findSquare 100 p))
  val solve = solution o Intcode.load o Reader.readFromFile
end
