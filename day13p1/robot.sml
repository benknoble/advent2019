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

  fun colorToChar (c : color) : char =
    case c of
         B => #" "
       | W => #"#"

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
  val newHull : hull = PointMap.insert (PointMap.empty, P.origin, W)

  fun colorOf (h : hull) (p : P.point) : color =
    case PointMap.find (h,p) of
         NONE => B
       | SOME c => c

  fun paint (h : hull) (p : P.point) (c : color) : hull =
    PointMap.insert (h, p, c)

  fun paint' (h : hull) (p : P.point) (i : int) : hull =
    paint h p (colorFromInt i)

  fun paint'' (h : hull) (r : robot) (i : int) : hull =
    paint' h (#1 r) i

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
    let val ran = Intcode.run p
    in if Intcode.stopped ran then h
       else
         let
           val (inReq,_,_) = ran
           val col = getOutput r h
           val p' = Intcode.appIn inReq col
           val (out,_,_) = Intcode.run p'
           val (newCol, p'') = Intcode.getOut out
           val h' = paint'' h r newCol
           val (out',_,_) = Intcode.run p''
           val (newDir, p''') = Intcode.getOut out'
           val r' = step r newDir
         in
           run r' h' p'''
         end
    end

  fun hullToString (h : hull) : string =
    let
      val pcs = PointMap.listItemsi h
      val whites = List.filter (fn (_,c) => c = W) pcs
      val minX = foldl (fn ((p,_),x) => Int.min (x, #x p)) 0 whites
      val maxX = foldl (fn ((p,_),x) => Int.max (x, #x p)) 0 whites
      val nCols = maxX - minX + 1
      val minY = foldl (fn ((p,_),y) => Int.min (y, #y p)) 0 whites
      val maxY = foldl (fn ((p,_),y) => Int.max (y, #y p)) 0 whites
      val nRows = maxY - minY + 1
      val grid = Vector.tabulate (nRows, (fn i =>
                 Vector.tabulate (nCols+1, (fn j =>
                 if j = nCols then #"\n"
                 else colorToChar B))))
      fun placeWhite grid pt =
        let
          val {x,y} = pt
          val (x', y') = (x-minX, y-minY)
        in
          Vector.update (grid, y',
          Vector.update (Vector.sub (grid, y'), x',
          colorToChar W))
        end
      val charsV = foldl (fn (p,g) => placeWhite g p) grid (map #1 whites)
      val charsL' = Vector.map (Vector.foldr (op ::) []) charsV
      val charsL = Vector.foldr (op ::) [] charsL'
      val rows = map String.implode (List.rev charsL)
    in
      String.concat rows
    end
end
