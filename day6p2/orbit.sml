signature ORBITMAP = sig
  type node
  type orbitMap
  val empty : orbitMap
  val insert : orbitMap -> (node * node) -> orbitMap
  val build : (node * node) list -> orbitMap
  val pathLength : orbitMap -> node -> node -> int option
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
  type edges = Set.set
  fun update ma no va = case Map.find (ma, no) of
                             NONE => Map.insert (ma, no, Set.singleton va)
                           | SOME s => Map.insert (ma, no, (Set.add (s, va)))

  type orbitMap = edges Map.map * edges Map.map
  val empty = (Map.empty, Map.empty)
  fun insert (m1,m2) (k, v) =
    let
      (* attempt to handle "root" nodes by placing an empty set *)
      fun insEmIfNot m k v = case Map.find (m, k) of
                                  NONE => Map.insert (m, k, Set.empty)
                                | SOME _ => m
      val m1' = insEmIfNot m1 v k
      val m2' = insEmIfNot m2 k v
    in
      (update m1' k v, update m2' v k)
    end
  val build = foldl (fn (rel,ma) => insert ma rel) empty

  fun toList (m1,m2) =
    let
      fun toList' m = Map.listItemsi (Map.map Set.listItems m)
    in
      (toList' m1, toList' m2)
    end

  fun pathLength (m : orbitMap) src dst =
    let
      datatype color = WHITE | GRAY | BLACK
      type bfs = {col : color, dist : int option} Map.map
      fun bfs ((m1, m2) : orbitMap) s =
        let
          val (ks, vs) = ListPair.unzip (Map.listItemsi m1)
          val (ks', vs') = ListPair.unzip (Map.listItemsi m2)
          val init' = foldl (fn (k,m) =>
            if k = s then m
            else Map.insert (m, k, {col=WHITE, dist=NONE}))
            Map.empty
            ((ks @ (Set.listItems (foldl Set.union Set.empty vs)))
            @ (ks' @ (Set.listItems (foldl Set.union Set.empty vs))))
          val init = Map.insert (init', s, {col=GRAY, dist=SOME 0})
          fun step q tree =
            if Fifo.isEmpty q then tree
            else
              let
                val (q', u) = Fifo.dequeue q
                val dist = valOf (#dist (valOf (Map.find (tree, u))))
                val dist' = dist + 1
                val m1u = Map.find (m1, u)
                val m2u = Map.find (m2, u)
                val union = Option.map (fn lf =>
                            Option.map (fn rt => Set.union (lf, rt))
                            m2u)
                            m1u
              in case Option.join union of
                      NONE => step q' tree
                    | SOME vs =>
                        let val (q'', tree') =
                          foldl
                          (fn (v,(qu,tr)) =>
                            case Option.map #col (Map.find (tr, v)) of
                                 NONE => (qu, tr)
                               | SOME WHITE => (Fifo.enqueue (qu, v), Map.insert (tr, v, {col=GRAY, dist=SOME dist'}))
                               | _ => (qu,tr))
                          (q', tree)
                          (Set.listItems vs)
                        in step q'' (Map.insert (tree', u, {col=BLACK, dist=SOME dist}))
                        end
              end
        in step (Fifo.enqueue (Fifo.empty, s)) init
        end
      val res = (Option.join o Option.map #dist) (Map.find (bfs m src, dst))
    in Option.map (fn r => r - 2) res (* subtract start and finish *)
    end
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

fun solution om = OrbitMap.pathLength om "YOU" "SAN"
val solveStr = solution o Reader.read
val solve = solution o Reader.readFile
