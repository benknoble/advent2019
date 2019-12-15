structure Arcade = struct
  datatype tile = EMPTY
                | WALL
                | BLOCK
                | PADDLE
                | BALL

  exception BadTile of int
  fun intToTile (i : int) : tile =
    case i of
         0 => EMPTY
       | 1 => WALL
       | 2 => BLOCK
       | 3 => PADDLE
       | 4 => BALL
       | _ => raise BadTile i

  fun tileToChar (t : tile) : char =
    case t of
         EMPTY => #" "
       | WALL => #"*"
       | BLOCK => #"#"
       | PADDLE => #"-"
       | BALL => #"o"


  structure P = Point

  structure PointMap = RedBlackMapFn (
    type ord_key = P.point
    val compare = P.compare
  )

  type screen = tile PointMap.map

  val newScreen : screen = PointMap.empty

  fun tileOf (s : screen) (p : P.point) : tile =
    case PointMap.find (s,p) of
         NONE => EMPTY
       | SOME c => c

  fun draw (s : screen) (p : P.point) (t : tile) : screen =
    PointMap.insert (s, p, t)

  fun draw' (s : screen) (p : P.point) (i : int) : screen =
    draw s p (intToTile i)

  fun screenToString (s : screen) : string =
    let
      val pcs = PointMap.listItemsi s
      val minX = foldl (fn ((p,_),x) => Int.min (x, #x p)) 0 pcs
      val maxX = foldl (fn ((p,_),x) => Int.max (x, #x p)) 0 pcs
      val nCols = maxX - minX + 1
      val minY = foldl (fn ((p,_),y) => Int.min (y, #y p)) 0 pcs
      val maxY = foldl (fn ((p,_),y) => Int.max (y, #y p)) 0 pcs
      val nRows = maxY - minY + 1
      val grid = Vector.tabulate (nRows, (fn i =>
                 Vector.tabulate (nCols+1, (fn j =>
                 if j = nCols then #"\n"
                 else tileToChar EMPTY))))
      fun place grid pt =
        let
          val {x,y} = pt
          val (x', y') = (x-minX, y-minY)
        in
          Vector.update (grid, y',
          Vector.update (Vector.sub (grid, y'), x',
          tileToChar (tileOf s pt)))
        end
      val charsV = foldl (fn (p,g) => place g p) grid (map #1 pcs)
      val charsL' = Vector.map (Vector.foldr (op ::) []) charsV
      val charsL = Vector.foldr (op ::) [] charsL'
      val rows = map (String.implode o rev) charsL
    in
      String.concat rows
    end

  fun run (s : screen) (p : Intcode.process) : screen =
    let val ran = Intcode.run p
    in if Intcode.stopped ran then s
       else
         let
           val (outX,_,_) = ran
           val (x, p') = Intcode.getOut outX
           val (outY,_,_) = Intcode.run p'
           val (y, p'') = Intcode.getOut outY
           val (outT,_,_) = Intcode.run p''
           val (t, p''') = Intcode.getOut outT
           val s' = draw' s (Point.new x y) t
         in
           (* print (screenToString s); *)
           (* OS.Process.sleep (Time.fromReal 0.3); *)
           run s' p'''
         end
    end
end
