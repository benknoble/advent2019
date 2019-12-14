structure Moon = struct
  type pos = {x : int, y : int, z : int}
  type vel = {x' : int, y' : int, z' : int}

  fun sumVel (v1 : vel) (v2 : vel) =
      {x'=(#x' v1 + #x' v2)
      ,y'=(#y' v1 + #y' v2)
      ,z'=(#z' v1 + #z' v2)}

  type moon = pos * vel

  fun new (x : int) (y : int) (z : int) : moon =
    ({x=x, y=y, z=z}, {x'=0, y'=0, z'=0})

  fun applyVel (m : moon) : moon =
    let val ({x, y, z},  {x', y', z'}) = m
    in ({x=x+x', y=y+y', z=z+z'},  {x'=x', y'=y', z'=z'})
    end

  fun applyGrav (m1 : moon) (m2 : moon) : moon =
    let
      val {x=x1, y=y1, z=z1} = #1 m1
      val {x=x2, y=y2, z=z2} = #1 m2
      val {x', y', z'} = #2 m1
      val x'' = Int.sign (x2 - x1)
      val y'' = Int.sign (y2 - y1)
      val z'' = Int.sign (z2 - z1)
    in
      (#1 m1, {x'=x'', y'=y'', z'=z''})
    end

  fun pot ((p,_) : moon) : int =
    (abs (#x p))
    + (abs (#y p))
    + (abs (#z p))

  fun kin ((_,v) : moon) : int =
    (abs (#x' v))
    + (abs (#y' v))
    + (abs (#z' v))

  fun tot (m : moon) : int = pot m * kin m
end

structure NBody = struct
  structure M = Moon
  type moons = M.moon list

  fun step (moons : moons) : moons =
    let
      val deltaV = ListXProd.mapX (fn (a,b) => M.applyGrav a b) (moons, moons)
      val vByMoon = Eq.classes (fn a => fn b => #1 a = #1 b) deltaV
      val reduceV = map (fn c => foldl
                        (fn ((p,v),(_,v')) => (p, M.sumVel v v'))
                        (valOf (List.find (fn m => #1 m = #1 (hd c)) moons))
                        c)
                    vByMoon
    in
      map M.applyVel reduceV
    end

  fun stepN (n : int) (moons : moons) : moons =
    if n = 0 then moons
    else stepN (n-1) (step moons)

  fun tot (moons : moons) : int =
    foldl (op +) 0
    (map M.tot moons)
end
