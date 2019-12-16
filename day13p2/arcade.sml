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
       | WALL => #"#"
       | BLOCK => #"="
       | PADDLE => #"^"
       | BALL => #"O"


  structure P = Point

  structure PointMap = RedBlackMapFn (
    type ord_key = P.point
    val compare = P.compare
  )

  type screen = tile PointMap.map * int
  val newScreen : screen = (PointMap.empty, 0)
  val score : screen -> int = #2

  fun tileOf (s : screen) (p : P.point) : tile =
    case PointMap.find (#1 s,p) of
         NONE => EMPTY
       | SOME c => c

  fun draw (s : screen) (p : P.point) (t : tile) : screen =
    (PointMap.insert (#1 s, p, t), #2 s)

  fun draw' (s : screen) (p : P.point) (i : int) : screen =
    if p = Point.new ~1 0
    then (#1 s, i)
    else draw s p (intToTile i)

  fun screenToString (s : screen) : string =
    let
      val pcs = PointMap.listItemsi(#1  s)
      val minX = foldl (fn ((p,_),x) => Int.min (x, #x p)) 0 pcs
      val maxX = foldl (fn ((p,_),x) => Int.max (x, #x p)) 0 pcs
      val nCols = maxX - minX + 1
      val minY = foldl (fn ((p,_),y) => Int.min (y, #y p)) 0 pcs
      val maxY = foldl (fn ((p,_),y) => Int.max (y, #y p)) 0 pcs
      val nRows = maxY - minY + 1
      val grid = Vector.tabulate (nRows+1, (fn i =>
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
      val rows = map String.implode charsL
    in
      String.concat (rows @ ["SCORE\t" ^ Int.toString (score s) ^ "\n"])
    end

  datatype tilt = NEUTRAL
                | LEFT
                | RIGHT
  type joystick = screen -> tilt

  fun joystickStdIO _ : tilt =
    (print ">";
     case (hd o explode o TextIO.inputN) (TextIO.stdIn, 1) of
          #"L" => LEFT
        | #"l" => LEFT
        | #"R" => RIGHT
        | #"r" => RIGHT
        | _ => NEUTRAL)

  exception Borked of screen
  fun run (j : joystick) (s : screen) (p : Intcode.process) : screen =
    let val ran = Intcode.run p
    in if Intcode.stopped ran then s
       else if Intcode.isOut ran then
         let
           val (outX,_,_) = ran
           val (x, p') = Intcode.getOut outX
           val (outY,_,_) = Intcode.run p'
           val (y, p'') = Intcode.getOut outY
           val (outT,_,_) = Intcode.run p''
           val (t, p''') = Intcode.getOut outT
           val s' = draw' s (Point.new x y) t
         in
           run j s' p'''
         end
      else if Intcode.isIn ran then
        (print (screenToString s);
        (* OS.Process.sleep (Time.fromReal 0.3); *)
        let
          val (inReq,_,_) = ran
          val tilt = j s
          val input = case tilt of
                           NEUTRAL => 0
                         | LEFT => ~1
                         | RIGHT => 1
          val cont = Intcode.appIn inReq input
        in
          run j s cont
        end)
      else raise Borked s
    end

  exception WontTakeQuarters
  fun insertQtrs (p : Intcode.program) (n : int) : Intcode.program =
    Memory.tryWrite
    (fn _ => raise WontTakeQuarters)
    (fn succ => succ)
    (Memory.write p 0 n)
end
