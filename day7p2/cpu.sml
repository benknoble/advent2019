signature CPU = sig
  type elem
  type instream
  type outstream
  type program
  type process
  type result
  val load : program -> process
  val run : process -> instream -> outstream -> result
  (* usually run o load *)
  val interpret : program -> instream -> outstream -> result
end

functor CPUFn (structure Memory : MEMORY
               structure Decoder : DECODER sharing Decoder = Memory)
               : CPU = struct
  type elem = Memory.elem
  type instream = unit -> elem
  type outstream = elem -> unit
  datatype state = RUNNING
                 | MEM_R_ERR of Memory.addr
                 | MEM_W_ERR of Memory.addr * Memory.elem
                 | UNKNOWN_ERR of Memory.addr * Decoder.opcode
                 | FINISHED
  val init = Memory.base
  type program = Memory.memory
  type process = state * Memory.memory * Memory.addr
  type result = state * Memory.memory * Memory.addr

  val eta = Memory.elemToAddr
  val tryRead = Memory.tryRead MEM_R_ERR
  val tryWrite = Memory.tryWrite MEM_W_ERR

  structure Eval = struct
    val tryRead =
      Memory.tryRead (fn e => fn (m, a) =>
      (MEM_R_ERR e, m, a) : process)
    val tryWrite =
      Memory.tryWrite (fn e => fn (m, a) =>
      (MEM_W_ERR e, m, a) : process)
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
          (RUNNING, w, newIp))
        ((#3 dest) (f (a,b))) (m'', a''))
        ((#3 srcB)()) (m', a'))
        ((#3 srcA)()) (m, ip)
      end

    val add = arithOp Memory.add
    val mult = arithOp Memory.mult
    fun input
      (ins : instream)
      (m : Memory.memory)
      (ip : Memory.addr)
      (u : Decoder.unaryW)
      : process =
      let val {addr, newIp} = Decoder.unaryWToRec u
      in
      tryWrite (fn w => fn _ => (RUNNING, w, newIp))
               ((#3 addr) (ins())) (m, ip)
      end
    fun output
      (outs : outstream)
      (m : Memory.memory)
      (ip : Memory.addr)
      (u : Decoder.unaryR)
      : process =
      let val {addr, newIp} = Decoder.unaryRToRec u
      in
      tryRead (fn e => fn _ => (outs e ; (RUNNING, m, newIp)))
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
          then tryRead (fn d => fn (m', a') => (RUNNING, m, (eta d)))
                       ((#3 jmpIp)()) (m, ip)
          else (RUNNING, m, defIp))
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
          (RUNNING, w, newIp))
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
        tryRead (fn rb => fn (m', a') => (RUNNING, Memory.setRelBase m' (eta rb), newIp))
        ((#3 addr)()) (m, ip)
      end
  end

  fun step ins outs proc = case proc of
    (RUNNING, m, ip) => (
      case Decoder.decode m ip of
           Decoder.ADD a => Eval.add m ip a
         | Decoder.MULT a => Eval.mult m ip a
         | Decoder.INPUT a => Eval.input ins m ip a
         | Decoder.OUTPUT a => Eval.output outs m ip a
         | Decoder.JMP_T a => Eval.jmp m ip a
         | Decoder.JMP_F a => Eval.jmp m ip a
         | Decoder.TST_LT a => Eval.tst m ip a
         | Decoder.TST_EQ a => Eval.tst m ip a
         | Decoder.SET_BASE a => Eval.setBase m ip a
         | Decoder.HALT => (FINISHED, m, ip)
         | Decoder.UNKNOWN_OP u => (UNKNOWN_ERR (ip, u), m, ip)
         | Decoder.UNKNOWN_MODE c => (UNKNOWN_ERR (ip, c), m, ip)
         | Decoder.MEM_ERR e => (MEM_R_ERR e, m, ip))
    | p => p
    (* all other states (finished, errors) are fixed points *)

  fun load p = (RUNNING, p, init)
  fun run (s, m, ip) ins outs =
    case s of
         RUNNING => run (step ins outs (s, m, ip)) ins outs
       | _ => (s, m, ip)
  val interpret = run o load
end
