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

  fun nthDestroyed (n : int) (map : map) (station : P.point)
    (* : P.point *)
    =
    let
      (* val woStation = List.filter (fn p => p<>station) map *)
      (* val moved = List.map *)
      (*             (P.move (P.map (fn (x,y) => Point.new (~x) (~y)) station)) *)
      (*             woStation *)
      (* fun sweep n ps = *)
      (*   let val visible = length (Eq.classes rel ps) *)
      (*   in if visible < n then sweep (n - visible) (List.drop (ps,visible)) *)
      (*      else (n, ps) *)
      (*   end *)
      (* val (n', rest) = sweep n moved *)
      (* val tagged = List.map (fn p => (p, P.quadOf p, P.slope P.origin p)) rest *)
      (* val sorted = ListMergeSort.sort *)
      (*              (fn ((_, q1, d1), (_, q2, d2)) => *)
      (*              if q1 = q2 then d1 > d2 *)
      (*              else P.clockwiseFromYP (q1, q2)) *)
      (*              tagged *)
      (* ==================================================================== *)
      (* val closest' = ListMergeSort.sort *)
      (*                (fn (p1, p2) => P.manhattan p1 station > P.manhattan p2 station) *)
      (*                map *)
      (* val closest = List.filter (fn p => p <> station) closest' *)
      (* val wAngle = List.map *)
      (*              (fn p => (p, P.map (fn (x,y) => P.map (fn (x0, y0) => *)
      (*                Math.atan2 (real (y-y0), real (x-x0))) station) p)) *)
      (*              closest *)
      (* val sortedAngle = ListMergeSort.sort *)
      (*                   (fn (p1, p2) => #2 p1 > #2 p2) *)
      (*                   wAngle *)
      (* val byAngle' = Eq.classes *)
      (*                (fn p1 => fn p2 => Real.== (#2 p1, #2 p2)) *)
      (*                sortedAngle *)
      (* val byAngle = List.rev (List.map List.rev byAngle') *)
      (* val points = List.map (List.map #1) byAngle *)
      (* fun lase ([] : P.point list list) = [] *)
      (*   | lase (h::t) = List.map hd (h::t) @ lase t *)
      (* val lased = lase points *)
    in
      (* List.nth (lased, n-1) *)
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
