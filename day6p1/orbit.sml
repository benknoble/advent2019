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

  val direct = fn _ => 0
  val indirect = fn _ => 0
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
