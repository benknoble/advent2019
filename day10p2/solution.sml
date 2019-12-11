structure Solution = struct
  fun solve s =
    let
      val map = AsteroidMap.read s
      val station = (#1 o valOf o AsteroidMap.idealMonitorLoc) map
    in
      AsteroidMap.nthDestroyed 200 map station
    end
end
