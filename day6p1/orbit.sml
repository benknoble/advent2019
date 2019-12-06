signature ORBITMAP = sig
  type node
  type orbitMap
  val empty : orbitMap
  val insert : orbitMap -> (node * node) -> orbitMap
  val build : (node * node) list -> orbitMap
  val direct : orbitMap -> int
  val indirect : orbitMap -> int
end

structure OrbitMap : ORBITMAP = struct
  type node = string

  (* Auxiliary structures *)
  structure Map = RedBlackMapFn (struct
    type ord_key = node
    val compare = String.compare
  end)
  structure Set = RedBlackSetFn (struct
    type ord_key = node
    val compare = String.compare
  end)
  type v = Set.set
  fun update ma no va = case Map.find (ma, no) of
                             NONE => Map.insert (ma, no, Set.singleton va)
                           | SOME s => Map.insert (ma, no, (Set.add (s, va)))

  type orbitMap = v Map.map
  val empty = Map.empty
  fun insert ma (k, v) = update ma k v
  val build = List.foldl (fn ((k,v),m) => update m k v) empty

  val mkVisited : orbitMap -> node list = (map #1) o Map.listItemsi
  val mkCounter = Map.mapi (fn (k : node, v : v) => 0)
  val sum = Map.foldl (op +) 0
  val hasUnvisited : node list -> bool = not o List.null
  fun counter f m =
    let
      val visited = mkVisited m
      val counters = mkCounter m
      fun count key vst ctr =
        let
          val cnt = f (Map.find (m, key))
          val ctr' = Map.insert (ctr, key, cnt)
        in
          ctr'
        end
      fun countAll vst ctr =
        if hasUnvisited vst
        then
          let
            val k = hd vst
            val vst' = tl vst
            val ctr' = count k vst ctr
          in
            countAll vst' ctr'
          end
        else ctr
    in
      sum (countAll visited counters)
    end
  val direct = counter (fn SOME s => Set.numItems s
                         | NONE => 0)
  val indirect = counter (fn _ => 0)
end

structure Reader = struct
  val read : string -> OrbitMap.orbitMap =
    let
      val toOrbits = String.tokens Char.isSpace
      val toLRs = map (String.tokens (fn c => c = #")"))
      val toPairs = map (fn lr => ((hd o tl) lr, hd lr))
    in
      OrbitMap.build o toPairs o toLRs o toOrbits
    end
  val readStream : TextIO.instream -> OrbitMap.orbitMap = read o TextIO.inputAll
  val readFile : string -> OrbitMap.orbitMap = readStream o TextIO.openIn
end

fun solution om = OrbitMap.direct om + OrbitMap.indirect om
val solveStr = solution o Reader.read
val solve = solution o Reader.readFile
