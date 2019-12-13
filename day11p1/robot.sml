structure Robot = struct
  datatype color = B | W
  datatype direction = L | U | R | D

  exception BadColor of int
  fun colorFromInt (i : int) : color =
    case i of
         0 => B
       | 1 => W
       | _ => raise BadColor i

  fun colorToInt (c : color) : int =
    case c of
         B => 0
       | W => 1

  exception BadRotation of int
  fun rotate (d : direction) (i : int) : direction =
    case (i,d) of
         (0,L) => D
       | (1,L) => U
       | (0,U) => L
       | (1,U) => R
       | (0,R) => U
       | (1,R) => D
       | (0,D) => R
       | (1,D) => L
       | _ => raise BadRotation i

  structure P = Point

  fun toOffset (d : direction) : P.point =
    case d of
         L => P.new ~1 0
       | U => P.new 0 1
       | R => P.new 1 0
       | D => P.new 0 ~1

  structure PointMap = RedBlackMapFn (
    type ord_key = P.point
    val compare = P.compare
  )

  type robot = P.point * direction
  type hull = color PointMap.map

  val newRobot : robot = (P.origin, U)
  val newHull : hull = PointMap.empty

  fun colorOf (h : hull) (p : P.point) : color =
    case PointMap.find (h,p) of
         NONE => B
       | SOME c => c

  fun paint (h : hull) (p : P.point) (c : color) : hull =
    PointMap.insert (h, p, c)

  fun paint' (h : hull) (p : P.point) (i : int) : hull =
    paint h p (colorFromInt i)

  fun step (r : robot) (i : int) : robot =
    let
      val dir' = rotate (#2 r) i
      val pos' = P.move (#1 r) (toOffset dir')
    in
      (pos', dir')
    end

  fun read (r : robot) (h : hull) : color =
    colorOf h (#1 r)

  fun getOutput (r : robot) (h : hull) : int =
    colorToInt (read r h)

  fun run (r : robot) (h : hull) (p : Intcode.process) : hull =
    (* TODO *)
    newHull

end
