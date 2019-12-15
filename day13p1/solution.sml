structure Solution = struct
  fun solution prog =
    let
      val screen = Arcade.newScreen
      val screenFinal = Arcade.run screen (Intcode.load prog)
      val blocks = Arcade.PointMap.filter (fn x => x = Arcade.BLOCK)
    in
      Arcade.PointMap.numItems (blocks screenFinal)
    end

  val solve = solution o Reader.readFromFile
end
