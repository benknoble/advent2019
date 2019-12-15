structure Solution = struct
  fun solution prog =
    let
      val robot = Robot.newRobot
      val hull = Robot.newHull
      val proc = Intcode.load prog
    in
      Robot.hullToString (Robot.run robot hull proc)
    end

  val solve = print o solution o Reader.readFromFile
end
