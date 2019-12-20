structure Tractor = struct
  fun isPulling (proc : Intcode.process) (p : Point.point) : bool =
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
      val points = map (fn (x,y) => Point.new x y) xys
      val pulls = map (isPulling proc) points
      val boolToInt = map (fn b => if b then 1 else 0) pulls
    in
      foldl (op +) 0 boolToInt
    end
end
