structure Solution = struct
  (* adapted from https://stackoverflow.com/a/46227650/4400820 *)
  fun perm lst =
    let
      infix ^^
      fun x ^^ ll = map (fn l => x::l) ll
      fun perm' lef rig =
        case rig of
            [] => [[]]
          | [x] => x ^^ (perm' [] lef)
          | x::t => let val s = perm' (x::lef) t
                    in (x ^^ perm' [] (lef @ t)) @ s
                    end
    in
      perm' [] lst
    end

  fun solution prog =
    let
      val phases = perm [5,6,7,8,9]
      val thrusters = map (Amplifier.loop 0 prog) phases
    in
      foldl Int.max ~1 thrusters
    end

  val solve = solution o Reader.readFromFile
end
