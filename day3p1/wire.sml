fun optionListToListOption (xs : 'a option list) : 'a list option =
  let
    fun f (NONE, _) = NONE
      | f (_, NONE) = NONE
      | f (SOME x, SOME xs') = SOME (x::xs')
  in
    Option.map rev (foldl f (SOME []) xs)
  end

structure Step = struct
  datatype direction = L | D | U | R
  type step = direction * int
  fun map (f : direction * int -> 'a) (s: step) : 'a =
    let val (d, i) = s
    in f(d, i)
    end

  fun fromString (s : string) : step option =
    let
      val toDist = Int.fromString
      fun toDir (c : char) = case c of
                                  #"L" => SOME L
                                | #"D" => SOME D
                                | #"U" => SOME U
                                | #"R" => SOME R
                                | _ => NONE
      val chars = explode s
      val (direction, length) = case chars of
                                     [] => (#"_", "")
                                   | c::r => (c, implode r)
    in
      case (toDir direction, toDist length) of
           (NONE, _) => NONE
         | (_, NONE) => NONE
         | (SOME d, SOME i) => SOME (d, i)
    end

end

structure Path = struct
  type path = Step.step list
  val map : (Step.step -> 'a) -> path -> 'a list = List.map

  fun fromString (s : string) : path option =
    let
      val tokens = String.tokens (fn c => c = #",") s
      val steps = List.map Step.fromString tokens
    in
      optionListToListOption steps
    end
end

structure Point = struct
  type point = {x : int, y : int}
  val origin : point = {x=0, y=0}

  fun map (f : int * int -> 'a) (p : point) : 'a =
    let val {x, y} = p
    in f (x, y)
    end

  fun compare (p1 : point, p2 : point) : order =
    map (fn (x, y) =>
        map (fn (x', y') => case Int.compare (x, x') of
                                 EQUAL => Int.compare(y, y')
                               | ord => ord)
        p2)
    p1

  fun map2
    (fx : int * int -> 'a)
    (fy : int * int -> 'b)
    (p1 : point)
    (p2 : point)
    : 'a * 'b =
    let
      val {x, y} = p1
      val {x=x', y=y'} = p2
    in
      (fx (x, x')
      ,fy (y, y'))
    end

  fun manhattan (p1 : point) (p2 : point) : int =
    let val lineLength = abs o (op -)
        val (dx, dy) = map2 lineLength lineLength p1 p2
    in dx + dy
    end

  val dist = manhattan
  val distToOrigin = dist origin

  fun move (p : point) (s : Step.step) : point =
    let
      fun move' (x, y) = Step.map (fn (d, i) => case d of
                                                     Step.L => {x=x-i, y=y}
                                                   | Step.D => {x=x, y=y-i}
                                                   | Step.U => {x=x, y=y+i}
                                                   | Step.R => {x=x+i, y=y})
    in map move' p s
    end

  fun trace (p : point) (s : Step.step) : point list =
    let
      fun trace'' acc (x, y) = Step.map (fn (d, i) =>
        if i = 0 then acc
        else
          let
            fun acc' p = p::acc
            val s' = (d, i-1)
            fun trace''' (x, y) = trace'' (acc' {x=x, y=y}) (x, y) s'
          in
            case d of
                Step.L => trace''' (x-1, y)
              | Step.D => trace''' (x, y-1)
              | Step.U => trace''' (x, y+1)
              | Step.R => trace''' (x+1, y)
          end)
      val trace' = trace'' [p]
    in
      map trace' p s
    end
end

(* requires a RedBlackSetFn implementation,
* such as automatically provided by smlnj
*)
structure PointSet = RedBlackSetFn (struct
  type ord_key = Point.point
  val compare = Point.compare
end)

structure Wires = struct
  type wire = Point.point list
  val map : (Point.point -> 'a) -> wire -> 'a list = List.map
  val find : (Point.point -> bool) -> wire -> Point.point option = List.find

  fun toWire (p : Path.path) : wire =
    let fun toWire' (st, []) = Point.trace Point.origin st
          | toWire' (st, acc::accs) = (Point.trace acc st) @ acc::accs
    in
      rev (foldl toWire' [] p)
    end

  (* for efficiency, this is implemented via an SML/NJ RedBlackSetFn
  *
  * the naïve list version has time complexity O(n^2) for lists of size n--
  * for two lists of size ~150,000, that's O(30,000,000)!
  *
  * if constructing a set from a list is O(n) in the size of the list,
  * and the intersection of two sets is O(n+m) in the size of the sets,
  * then the overall complexity of this function is O(w(n+m)) in the size of
  * the wire list--
  * for the same lists above, this is only O(300,000), two orders of magnitude
  * smaller
  *)
  fun intersections (ws : wire list) : Point.point list =
    let
      fun toSet (w : wire) = PointSet.addList (PointSet.empty, w)
      fun intersection (w, s) = PointSet.intersection ((toSet w), s)
    in
      case ws of
           [] => []
         | w::ws => PointSet.listItems (foldl intersection (toSet w) ws)
    end

  fun intersectionsWithout (p : Point.point) : wire list -> Point.point list =
    (List.filter (fn p' => p <> p')) o intersections

  val fromString : string -> wire option =
    (Option.map toWire) o Path.fromString
end

structure WireParser = struct
  val toPaths : string -> Path.path list option =
    optionListToListOption
    o (map Path.fromString)
    o (String.tokens Char.isSpace)

  val toWires : string -> Wires.wire list option =
    Option.map (map Wires.toWire) o toPaths

  val readWires : TextIO.instream -> Wires.wire list option =
    toWires o TextIO.inputAll

  val readWiresFromFile : string -> Wires.wire list option =
    readWires o TextIO.openIn
end

fun solution NONE = NONE
  | solution (SOME ws) =
    let
      val intersections = Wires.intersectionsWithout Point.origin ws
      val distances = map Point.distToOrigin intersections
      val base = if null distances then ~1 else hd distances
    in
      SOME (foldl (Int.min) base distances)
    end

val solve = solution o WireParser.readWiresFromFile
