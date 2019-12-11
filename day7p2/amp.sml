structure Amplifier =
struct
  fun mkReader phase reader =
    let
      val hasReadPhase = ref false
    in
      fn () =>
        (* this bang is unfortunate; it means "value of", as opposed to "not" *)
        if !hasReadPhase then reader()
        else (hasReadPhase := true; phase)
    end

  fun mkWriter (output : int ref) =
    fn i => output := i

  fun amplifierChain (init : unit -> int) (p : Intcode.program) (phases : int list) : int option =
    case phases of
        [] => NONE
      | hed::tal =>
      let
        val result = ref 0
        val start = mkReader hed init
        val outputs = map (fn ph => ref 0) tal
        val readers' = map (fn (out, t) => mkReader t (fn () => !out))
                          (ListPair.zip (outputs, tal))
        val readers = start::readers'
        val writers = map mkWriter outputs @ [mkWriter result]
        val amps = ListPair.zip (readers, writers)
        val proc = Intcode.load p
      in
        map (fn (r, w) => Intcode.run proc r w) amps ;
        SOME (!result)
      end
end
