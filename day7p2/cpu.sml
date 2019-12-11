signature CPU = sig
  type elem
  type program
  type process
  type result
  val load : program -> process
  val run : process -> result
  (* usually run o load *)
  val interpret : program -> result
end

functor CPUFn (structure Memory : MEMORY
               structure Decoder : DECODER sharing Decoder = Memory)
               : CPU = struct
  type elem = Memory.elem

  datatype state = RUNNING
                 | MEM_R_ERR of Memory.addr
                 | MEM_W_ERR of Memory.addr * Memory.elem
                 | UNKNOWN_ERR of Memory.addr * Decoder.opcode
                 | FINISHED
                 | IN of elem -> process
                 | OUT of elem
  (* mutual recursion means process *has* to be a datatype. boo. *)
  and process = P of state * Memory.memory * Memory.addr

  val init = Memory.base
  type program = Memory.memory
  type result = state * Memory.memory * Memory.addr

  val eta = Memory.elemToAddr
  val tryRead = Memory.tryRead MEM_R_ERR
  val tryWrite = Memory.tryWrite MEM_W_ERR

  structure Eval = struct
    val tryRead =
      Memory.tryRead (fn e => fn (m, a) =>
      P (MEM_R_ERR e, m, a))
    val tryWrite =
      Memory.tryWrite (fn e => fn (m, a) =>
      P (MEM_W_ERR e, m, a))
    fun arithOp
      (f : Memory.elem * Memory.elem -> Memory.elem)
      (m : Memory.memory)
      (ip : Memory.addr)
      (t : Decoder.ternary)
      : process =
      let val {srcA, srcB, dest, newIp} = Decoder.ternaryToRec t
      in
        tryRead (fn a => fn (m', a') =>
        tryRead (fn b => fn (m'', a'') =>
        tryWrite (fn w => fn _ =>
          P (RUNNING, w, newIp))
        ((#3 dest) (f (a,b))) (m'', a''))
        ((#3 srcB)()) (m', a'))
        ((#3 srcA)()) (m, ip)
      end

    val add = arithOp Memory.add
    val mult = arithOp Memory.mult
    fun input
      (m : Memory.memory)
      (ip : Memory.addr)
      (u : Decoder.unaryW)
      : process =
      let val {addr, newIp} = Decoder.unaryWToRec u
      in
        P (IN (fn e => tryWrite (fn w => fn _ => P (RUNNING, w, newIp))
                                ((#3 addr) e) (m, ip)),
           m, ip)
      end
    fun output
      (m : Memory.memory)
      (ip : Memory.addr)
      (u : Decoder.unaryR)
      : process =
      let val {addr, newIp} = Decoder.unaryRToRec u
      in
        tryRead (fn e => fn _ => P (OUT e, m, ip))
                ((#3 addr)()) (m, ip)
      end
    fun jmp
      (m : Memory.memory)
      (ip : Memory.addr)
      (j : Decoder.jmp)
      : process =
      let val {cmp, tst, jmpIp, defIp} = Decoder.condJmp j
      in
        tryRead (fn e => fn _ =>
          if cmp e
          then tryRead (fn d => fn (m', a') => P (RUNNING, m, (eta d)))
                       ((#3 jmpIp)()) (m, ip)
          else P (RUNNING, m, defIp))
        ((#3 tst)()) (m, ip)
      end
    fun tst
      (m : Memory.memory)
      (ip : Memory.addr)
      (t : Decoder.tst)
      : process =
      let val {cmp, tstA, tstB, res, ifT, ifF, newIp} = Decoder.test t
      in
        tryRead (fn a => fn (m', a') =>
        tryRead (fn b => fn (m'', a'') =>
        tryWrite (fn w => fn _ =>
          P (RUNNING, w, newIp))
        ((#3 res)(if cmp a b then ifT else ifF)) (m'', a''))
        ((#3 tstB)()) (m', a'))
        ((#3 tstA)()) (m, ip)
      end
    fun setBase
      (m : Memory.memory)
      (ip : Memory.addr)
      (u : Decoder.unaryR)
      : process =
      let val {addr, newIp} = Decoder.unaryRToRec u
      in
        tryRead (fn rb => fn (m', a') =>
                  P (RUNNING, Memory.setRelBase m' (eta rb), newIp))
                  ((#3 addr)()) (m, ip)
      end
  end

  fun step proc : process = case proc of
    P (RUNNING, m, ip) =>
      (case Decoder.decode m ip of
            Decoder.ADD a => Eval.add m ip a
          | Decoder.MULT a => Eval.mult m ip a
          | Decoder.INPUT a => Eval.input m ip a
          | Decoder.OUTPUT a => Eval.output m ip a
          | Decoder.JMP_T a => Eval.jmp m ip a
          | Decoder.JMP_F a => Eval.jmp m ip a
          | Decoder.TST_LT a => Eval.tst m ip a
          | Decoder.TST_EQ a => Eval.tst m ip a
          | Decoder.SET_BASE a => Eval.setBase m ip a
          | Decoder.HALT => P (FINISHED, m, ip)
          | Decoder.UNKNOWN_OP u => P (UNKNOWN_ERR (ip, u), m, ip)
          | Decoder.UNKNOWN_MODE c => P (UNKNOWN_ERR (ip, c), m, ip)
          | Decoder.MEM_ERR e => P (MEM_R_ERR e, m, ip))
    | p => p
    (* all other states (finished, errors) are fixed points *)

  fun load p = P (RUNNING, p, init)
  fun run (P (s, m, ip)) =
    case s of
         RUNNING => run (step (P (s, m, ip)))
       | _ => (s, m, ip)
  val interpret = run o load
end
