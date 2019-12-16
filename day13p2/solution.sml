structure Solution = struct
  fun solution prog =
    let
      val screen = Arcade.newScreen
      val qtrs = Arcade.insertQtrs prog 2
      val proc = Intcode.load qtrs
      val screenFinal = Arcade.run Arcade.joystickStdIO screen proc
    in
      Arcade.score screenFinal
    end

  val solve = solution o Reader.readFromFile
end
