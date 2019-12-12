structure Solution = struct
  fun solve s =
    let
      val map = AsteroidMap.read s
      val station = (#1 o valOf o AsteroidMap.idealMonitorLoc) map
    in
      Point.map (fn (x,y) => 100*x + y)
      (AsteroidMap.nthDestroyed 200 map station)
    end
end
