structure Solution = struct
  fun solution joy prog =
    let
      val screen = Arcade.newScreen
      val qtrs = Arcade.insertQtrs prog 2
      val proc = Intcode.load qtrs
      val screenFinal = Arcade.run joy screen proc
    in
      Arcade.score screenFinal
    end

  val manual = solution Arcade.joystickStdIO
  val ai = solution Arcade.joystickCPU

  val manualFile = manual o Reader.readFromFile
  val aiFile = ai o Reader.readFromFile

  val solve = aiFile
end
