structure Tractor = struct
  structure P = Point

  fun isPulling (proc : Intcode.process) (p : P.point) : bool =
    let
      val (inX,_,_) = Intcode.run proc
      val wX = Intcode.appIn inX (#x p)
      val (inY,_,_) = Intcode.run wX
      val wY = Intcode.appIn inY (#y p)
      val (out,_,_) = Intcode.run wY
      val (output,_) = Intcode.getOut out
    in
      output = 1
    end

  fun countPullingRange
    (proc : Intcode.process)
    (min : int)
    (max : int)
    : int =
    let
      val xs = List.tabulate ((max - min), fn i => i)
      val ys = xs
      fun cartesian xs ys = List.concat (map (fn x => map (fn y => (x,y)) ys) xs)
      val xys = cartesian xs ys
      val points = map (fn (x,y) => P.new x y) xys
      val pulls = map (isPulling proc) points
      val boolToInt = map (fn b => if b then 1 else 0) pulls
    in
      foldl (op +) 0 boolToInt
    end

  fun containsSquare
    (n : int)
    (proc : Intcode.process)
    (topleft : P.point)
    : bool =
    let
      val offset = n - 1
      val bottomright = P.move topleft (P.new offset (~offset))
      val isPulling' = isPulling proc
    in
      isPulling' topleft
      andalso isPulling' bottomright
    end

  fun findSquare (size : int) (proc : Intcode.process) : P.point list =
    let
      fun findSquare' x y =
        let
          fun findColumn x =
            if isPulling (proc) (P.new x y)
            then x
            else findColumn (x+1)
          val x' = findColumn x
          val point = P.new x' y
        in if containsSquare size proc point
           then point
           else findSquare' x' (y+1)
        end
      val first100 = List.tabulate (100, P.pi')
      val first100Pulled = List.filter (isPulling proc) first100
      val pulledNotOrigin = List.filter (fn p => p <> P.origin) first100Pulled
      val first = hd pulledNotOrigin
      val firstX = #x first
      val firstY = #y first
    in
      P.pointsInSquare size (findSquare' firstX firstY)
    end

  fun closestToTractor (sq : P.point list) : P.point option =
    if null sq then NONE
    else
      SOME (foldl (fn (c,a) => if P.distToOrigin c < P.distToOrigin a
                               then c
                               else a)
            (hd sq)
            (tl sq))
end
