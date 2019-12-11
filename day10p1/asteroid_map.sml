structure AsteroidMap = struct
  structure P = Point

  type map = P.point list

  fun idealMonitorLoc (map : map) : (P.point * int) option =
    if null map then NONE
    else
      let
        fun rel p1 p2 =
          P.onSameSlopeToOrigin p1 p2 andalso
          P.inSameQuadrant p1 p2
        fun moveRel p ps = List.map (P.move p) ps
        fun toDir p = P.map (fn (x,y) => P.new (~x) (~y)) p
        fun numberSeen p =
          let val notP = List.filter (fn p' => p'<>p) map
              val relP = moveRel (toDir p) notP
          in length (Eq.classes rel relP)
          end
        val seenEach = List.map (fn p => (p, numberSeen p)) map
      in
        SOME (foldl (fn ((p,s),(p',m)) => if s > m then (p,s) else (p',m))
              (hd seenEach) seenEach)
      end

  fun fromString s : map =
    let
      val lines' = String.tokens (fn c => c = #"\n") s
      val lines = List.map (String.explode) lines'
      val isAst = fn c => c = #"#"
      fun readRow row x y acc =
        case row of
             [] => acc
           | hd::tl =>
               let val acc' = if isAst hd then (Point.new x y)::acc else acc
               in readRow tl (x+1) y acc'
               end
      fun readLines lines =
        foldl (fn (l, (y, acc)) => (y+1, readRow l 0 y acc)) (0,[]) lines
    in
      #2 (readLines lines)
    end

  val read = fromString o TextIO.inputAll o TextIO.openIn
end
