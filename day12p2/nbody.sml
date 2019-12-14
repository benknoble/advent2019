structure Moon = struct
  type pos = {x : int, y : int, z : int}
  type vel = {x' : int, y' : int, z' : int}

  fun sumVel (v1 : vel) (v2 : vel) =
      {x'=(#x' v1 + #x' v2)
      ,y'=(#y' v1 + #y' v2)
      ,z'=(#z' v1 + #z' v2)}

  type moon = pos * vel

  fun x (m : moon) = #x (#1 m)
  fun y (m : moon) = #y (#1 m)
  fun z (m : moon) = #z (#1 m)
  fun x' (m : moon) = #x' (#2 m)
  fun y' (m : moon) = #y' (#2 m)
  fun z' (m : moon) = #z' (#2 m)

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

  fun pot (m : moon) : int =
    (abs (x m))
    + (abs (y m))
    + (abs (z m))

  fun kin (m : moon) : int =
    (abs (x' m))
    + (abs (y' m))
    + (abs (z' m))

  fun tot (m : moon) : int = pot m * kin m

  fun fromString s =
    let
      val toks = String.tokens (fn c => c = #",") s
      val toks' = map (String.tokens (fn c => c = #"=")) toks
      val toks'' = map (map (String.tokens (fn c => c = #"<" orelse c = #">"))) toks'
      val ints = map (map (List.mapPartial Int.fromString)) toks''
      val [x, y, z] = (List.concat o List.concat) ints
    in
      new x y z
    end
end

structure MultDiv = struct
  fun gcd u v =
    if v <> 0 then gcd v (u mod v)
    else (abs u)

  fun lcm u v =
    if u = 0 orelse v = 0 then 0
    else abs (u * v) div (gcd u v)
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

  fun fromString s =
    let val lines = String.tokens (fn c => c = #"\n") s
    in map M.fromString lines
    end

  structure StateSet = RedBlackSetFn (
    type ord_key = int * int
    fun compare (s : int * int, s' : int * int) =
      case Int.compare (#1 s, #1 s') of
           EQUAL => Int.compare (#2 s, #2 s')
         | ord => ord
  )

  type dimstate = StateSet.set
  fun toDimState (pos : M.moon -> int) (vel : M.moon -> int) (moons : moons)
    : dimstate =
    StateSet.addList (StateSet.empty, map (fn m => (pos m, vel m)) moons)

  fun dimStateEq (d1 : dimstate) (d2 : dimstate) = StateSet.equal (d1, d2)

  val xState = toDimState M.x M.x'
  val yState = toDimState M.y M.y'
  val zState = toDimState M.z M.z'

  type period = int * bool
  type periods = int * int * int

  fun findPeriods
    (n : int)
    (xs : dimstate)
    (ys : dimstate)
    (zs : dimstate)
    (xp : period)
    (yp : period)
    (zp : period)
    (moons : moons)
    : periods =
    if List.all #2 [xp,yp,zp]
    then (#1 xp, #1 yp, #1 zp)
    else
      let
        val moons' = step moons
        val (xs', ys', zs') = (xState moons', yState moons', zState moons')
        val xp' = if not (#2 xp) andalso dimStateEq xs' xs then (n,true) else xp
        val yp' = if not (#2 yp) andalso dimStateEq ys' ys then (n,true) else yp
        val zp' = if not (#2 zp) andalso dimStateEq zs' zs then (n,true) else zp
      in
        findPeriods (n+1) xs ys zs xp' yp' zp' moons'
      end

  fun findPeriod (moons : moons) : int =
    let
      val (xs, ys, zs) = (xState moons, yState moons, zState moons)
      val moons' = step moons
      val init = (~1, false)
      val (x,y,z) = findPeriods 1 xs ys zs init init init moons
    in MultDiv.lcm x (MultDiv.lcm y z)
   end
end
