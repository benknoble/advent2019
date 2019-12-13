structure Amplifier = struct
  fun loop
    (init : int)
    (p : Intcode.program)
    (phases : int list)
    : int =
    let
      (* all left at "input phase" stage *)
      val amps = map (fn _ => Intcode.interpret p) phases
      val zipped = ListPair.zip (amps, phases)
      (* now, procs waiting to be run *)
      val wPhases = map (fn ((s,_,_), p) => Intcode.appIn s p) zipped
      (* used to sync io; could do it with a loop var, but eh *)
      val storage = ref init
      fun feedbackLoop procs =
        let
          val paused = map Intcode.run procs
        in
          if List.all Intcode.stopped paused
          then !storage
          else (* should be in input or output stage; if prog correct, input *)
          feedbackLoop
            (map (fn (s,_,_) =>
            let
              val toRun = Intcode.appIn s (!storage)
              val (ran,_,_) = Intcode.run toRun
              (* now it has produced output *)
              val (out, next) = Intcode.getOut ran
            in storage := out; next
            end) paused)
        end
    in
      feedbackLoop wPhases
    end
end
