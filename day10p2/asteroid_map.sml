structure AsteroidMap = struct
  structure P = Point

  type map = P.point list

  fun rel p1 p2 =
    P.onSameSlopeToOrigin p1 p2 andalso
    P.inSameQuadrant p1 p2

  fun groupBySeen (map : map) (station : P.point) : P.point list list =
    let
      fun moveRel p ps = List.map (P.move p) ps
      fun toDir p = P.map (fn (x,y) => P.new (~x) (~y)) p
      fun seen p =
        let val notP = List.filter (fn p' => p'<>p) map
            val relP = moveRel (toDir p) notP
        in Eq.classes rel relP
        end
    in
      seen station
    end

  fun idealMonitorLoc (map : map) : (P.point * int) option =
    if null map then NONE
    else
      let
        fun numberSeen p = length (groupBySeen map p)
        val seenEach = List.map (fn p => (p, numberSeen p)) map
      in
        SOME (foldl (fn ((p,s),(p',m)) => if s > m then (p,s) else (p',m))
              (hd seenEach) seenEach)
      end

  fun laserOrder (map : map) (station : P.point) : P.point list =
    let
      val groups = groupBySeen map station
      val byAngle = ListMergeSort.sort (fn (g1, g2) =>
                    let
                      val (h1, h2) = (hd g1, hd g2)
                      (* Because of coordinate system, and because laser points,
                       * up, we transform (x,y) |-> (y,-x)
                       *
                       * This has the effect of making the angles sort
                       * correctly, starting from 0, if we consider (-y) to be 0
                       * in the original coordinate system--for the laser points
                       * up on the grid, which is in the (-y) direction relative
                       * to the station! It also sweeps clockwise...
                       *
                       * The only adjustment to make is that atan2 sends the
                       * negative-y-axis to π, which is "largest," while I need
                       * it to go to (-π), or "smallest." We handle this case
                       * specially.
                       *)
                      val flipped = P.map (fn (x,y) => P.new y (~x))
                      val (h1', h2') = (flipped h1, flipped h2)
                      val toA = P.map (fn (x,y) => Math.atan2 (real y, real x))
                      val (a1, a2) = (toA h1', toA h2')
                      val onYN = P.map (fn (x,y) => x = 0 andalso y < 0)
                    in
                      if onYN h1 then false
                      else onYN h2 orelse a1 > a2
                    end) groups
      val byDist = List.map (fn g =>
                   ListMergeSort.sort (fn (p1, p2) =>
                   (* dist to origin here, since that's where station is located
                    *
                    * manhattan is good enough: they are all on the same angle,
                    * so we cannot have (x,y) and (y,x) in the same group
                    *)
                   P.manhattan p1 P.origin > P.manhattan p2 P.origin)
                   g)
                   byAngle
      fun lase ([] : P.point list list) = []
        | lase ls = List.map hd ls @ lase (List.filter (not o null) (List.map tl ls))
    in
      List.map (P.move station) (lase byDist)
    end

  fun nthDestroyed (n : int) (map : map) (station : P.point) : P.point =
      List.nth (laserOrder map station, n-1)

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
