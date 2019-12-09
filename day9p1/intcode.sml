(* a right-biased either type, based on scala.util.Either *)
structure Either = struct
  datatype ('a, 'b) either = L of 'a
                           | R of 'b
  fun fold (l : 'a -> 'c) (r : 'b -> 'c) (e : ('a, 'b) either) : 'c =
    case e of
         L a => l a
       | R b => r b
  fun map (f : 'b -> 'c) : ('a, 'b) either -> ('a, 'c) either =
    fold (fn a => L a) (fn b => R (f b))
  fun flatMap (f : 'b -> ('a, 'b) either) : ('a, 'b) either -> ('a, 'b) either =
    fold (fn a => L a) (fn b => f b)
end

structure Utils = struct
  fun digits (i : int) : int list =
  let
    fun digits' i' acc =
      case i' of
            0 => acc
          | i'' => digits' (i'' div 10) ((i'' mod 10)::acc)
  in
    digits' i []
  end

  val fromDigits = List.foldl (fn (d, i) => i*10 + d) 0

  fun pad (n : int) (z : 'a) (xs : 'a list) : 'a list =
    let val length = length xs
    in
      if length >= n then xs
      else (List.tabulate (n - length,(fn i => z))) @ xs
    end

  fun padRight n z xs = rev (pad n z (rev xs))
end

signature IO = sig
  type elem
  val reader : unit -> elem
  val writer : elem -> unit
end

signature MEMORY = sig
  type elem
  type addr
  type memory

  val base : addr

  val read : memory -> addr -> (addr, elem) Either.either
  val write : memory -> addr -> elem -> (addr * elem, memory) Either.either
  val tryRead : (addr -> 'a) -> (elem -> 'a)
                -> ((addr, elem) Either.either -> 'a)
  val tryWrite : (addr * elem -> 'a) -> (memory -> 'a)
                 -> ((addr * elem, memory) Either.either -> 'a)

  val getRelToBase : memory -> addr -> addr
  val setRelBase : memory -> addr -> memory

  val nextIP : int -> addr -> addr

  val nextN : int -> addr -> memory -> (addr, elem) Either.either
  val next : addr -> memory -> (addr, elem) Either.either
  val next2 : addr -> memory -> (addr, elem) Either.either
  val next3 : addr -> memory -> (addr, elem) Either.either

  val elemToAddr : elem -> addr
  val addrToElem : addr -> elem

  val add : elem * elem -> elem
  val mult : elem * elem -> elem
end

signature DECODER = sig
  type opcode
  type elem
  type memory
  type addr

  datatype mode = POS
                | IMM
                | REL
                | UNKNOWN

  type param = addr * mode * (unit -> (addr, elem) Either.either)
  type dest = addr * mode * (elem -> (addr * elem, memory) Either.either)

  type unaryR
  type unaryW
  type jmp
  type tst
  type ternary
  val unaryRToRec : unaryR -> { addr : param
                              , newIp : addr
                              }
  val unaryWToRec : unaryW -> { addr : dest
                              , newIp : addr
                              }
  val condJmp : jmp -> { cmp : elem -> bool
                       , tst : param
                       , jmpIp : param
                       , defIp : addr
                       }
  val test : tst -> { cmp : elem -> elem -> bool
                    , tstA : param
                    , tstB : param
                    , res : dest
                    , ifT : elem
                    , ifF : elem
                    , newIp : addr
                    }
  val ternaryToRec : ternary -> { srcA : param
                                , srcB : param
                                , dest : dest
                                , newIp : addr
                                }

  datatype inst = ADD of ternary
                | MULT of ternary
                | INPUT of unaryW
                | OUTPUT of unaryR
                | JMP_T of jmp
                | JMP_F of jmp
                | TST_LT of tst
                | TST_EQ of tst
                | SET_BASE of unaryR
                | HALT
                | UNKNOWN_OP of opcode
                | UNKNOWN_MODE of opcode
                | MEM_ERR of addr

  val decode : memory -> addr -> inst
end

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

structure Memory : MEMORY = struct
  structure E = Either
  val L = E.L
  val R = E.R

  type elem = int
  type addr = int
  type memory = int * elem list

  val base = 0

  type readSucc = elem
  type readErr = addr
  type writeSucc = memory
  type writeErr = addr * elem
  type readRes = (readErr, readSucc) E.either
  type writeRes = (writeErr, writeSucc) E.either

  fun read (rb,m) p =
    let val m' = if length m > p then m else Utils.padRight (p+1) 0 m
    in
      R (List.nth (m', p))
      handle
      Subscript => L p
    end
  fun read' p m = read m p

  fun write (rb,m) p e =
    let val m' = if length m > p then m else Utils.padRight (p+1) 0 m
    in
      R (rb, (List.take (m', p)) @ [e] @ (List.drop (m', p+1)))
      handle
      Subscript => L (p, e)
    end
  fun write' p e m = write m p e

  val tryRead = E.fold
  val tryWrite = E.fold

  fun getRelToBase (rb, m) n = rb+n
  fun setRelBase (rb, m) n = (rb+n, m)

  fun nextIP n ip = ip + n

  fun nextN n curr : memory -> readRes = read' (curr + n)
  val next = nextN 1
  val next2 = nextN 2
  val next3 = nextN 3

  fun elemToAddr e = e
  fun addrToElem e = e

  val add = op +
  val mult = op *
end

functor IntDecoderFn (Memory : MEMORY where type elem = int) : DECODER = struct
  type opcode = Memory.elem
  type elem = Memory.elem
  type memory = Memory.memory
  type addr = Memory.addr

  datatype mode = POS
                | IMM
                | REL
                | UNKNOWN
  type param = addr * mode * (unit -> (addr, elem) Either.either)
  type dest = addr * mode * (elem -> (addr * elem, memory) Either.either)

  type unaryR = {
    addr : param
    , newIp : addr
    }
  type unaryW = {
    addr : dest
    , newIp : addr
    }
  type jmp = {
    cmp : elem -> bool
    , tst : param
    , jmpIp : param
    , defIp : addr
    }
  type tst = {
    cmp : elem -> elem -> bool
    , tstA : param
    , tstB : param
    , res : dest
    , ifT : elem
    , ifF : elem
    , newIp : addr
  }
  type ternary = {
    srcA : param
    , srcB : param
    , dest : dest
    , newIp : addr
    }
  val id = fn id => id
  val unaryRToRec = id
  val unaryWToRec = id
  val condJmp = id
  val test = id
  val ternaryToRec = id

  datatype inst = ADD of ternary
                | MULT of ternary
                | INPUT of unaryW
                | OUTPUT of unaryR
                | JMP_T of jmp
                | JMP_F of jmp
                | TST_LT of tst
                | TST_EQ of tst
                | SET_BASE of unaryR
                | HALT
                | UNKNOWN_OP of opcode
                | UNKNOWN_MODE of opcode
                | MEM_ERR of Memory.addr

  val eta = Memory.elemToAddr
  val ate = Memory.addrToElem
  val tryRead = Memory.tryRead MEM_ERR

  fun createParam a m mem : param =
    (a
    ,m
    ,case m of
          POS => (fn () => Memory.read mem a)
        | IMM => (fn () => Either.R (Memory.addrToElem a))
        | REL => (fn () => Memory.read mem (Memory.getRelToBase mem a))
        | UNKNOWN => (fn () => Either.L a))

  fun createDest (a : addr) m mem : dest =
    (a
    ,m
    ,case m of
          POS => Memory.write mem a
          (* for a destination, is there a difference?
          *  this version says: write to the address named
          *)
        | IMM => Memory.write mem a
        (* this version says: write the to the address named by reading the
        * argument
        *)
        (* | IMM => (fn e => Memory.tryRead *)
        (*                     (fn e' => Either.L (e', e)) *)
        (*                     (fn a' => Memory.write mem (eta a') e) *)
        (*                     (Memory.read mem a)) *)
        | REL => Memory.write mem (Memory.getRelToBase mem a)
        | UNKNOWN => (fn e => Either.L (a, e)))

  fun createUnaryR
    (f : unaryR -> inst)
    (m : memory)
    (ip : addr)
    (modes : mode list)
    : inst =
    let val newIp = Memory.nextIP 2 ip
    in tryRead (fn a =>
      f {addr=(createParam (eta a) (hd modes) m), newIp=newIp})
      (Memory.next ip m)
    end
  fun createUnaryW
    (f : unaryW -> inst)
    (m : memory)
    (ip : addr)
    (modes : mode list)
    : inst =
    let val newIp = Memory.nextIP 2 ip
    in tryRead (fn a =>
      f {addr=(createDest (eta a) (hd modes) m), newIp=newIp})
      (Memory.next ip m)
    end
  fun createTernary
    (f : ternary -> inst)
    (m : memory)
    (ip : addr)
    (modes : mode list)
    : inst =
    tryRead (fn a =>
    tryRead (fn b =>
    tryRead (fn d =>
      let
        val newIp = Memory.nextIP 4 ip
        val (am, bm, dm) = (hd modes, (hd o tl) modes, (hd o tl o tl) modes)
        val a' = createParam (eta a) am m
        val b' = createParam (eta b) bm m
        val d' = createDest (eta d) dm m
      in
        f {srcA=a', srcB=b', dest=d', newIp=newIp}
      end)
    (Memory.next3 ip m))
    (Memory.next2 ip m))
    (Memory.next ip m)
  fun createJmp
    (cmp : elem -> bool)
    (f : jmp -> inst)
    (m : memory)
    (ip : addr)
    (modes : mode list)
    : inst =
    tryRead (fn t =>
    tryRead (fn j =>
      let
        val defIp = Memory.nextIP 3 ip
        val (tm, jm) = (hd modes, (hd o tl) modes)
        val t' = createParam (eta t) tm m
        val j' = createParam (eta j) jm m
      in
        f {cmp=cmp, tst=t', jmpIp=j', defIp=defIp}
      end)
    (Memory.next2 ip m))
    (Memory.next ip m)
  fun createTst
    (cmp : elem -> elem -> bool)
    (f : tst -> inst)
    (m : memory)
    (ip : addr)
    (modes : mode list)
    : inst =
    tryRead (fn ta =>
    tryRead (fn tb =>
    tryRead (fn r =>
      let
        val newIp = Memory.nextIP 4 ip
        val (tam, tbm, rm) = (hd modes, (hd o tl) modes, (hd o tl o tl) modes)
        val ta' = createParam (eta ta) tam m
        val tb' = createParam (eta tb) tbm m
        val r' = createDest (eta r) rm m
      in
        f {cmp=cmp, tstA=ta', tstB=tb', res=r', ifT=1, ifF=0, newIp=newIp}
      end)
    (Memory.next3 ip m))
    (Memory.next2 ip m))
    (Memory.next ip m)

  fun toMode (i : opcode) : mode =
    case i of
         0 => POS
       | 1 => IMM
       | 2 => REL
       | _ => UNKNOWN

  fun opToInst (code : opcode) : opcode * mode list =
    let
      (* padding is to account for, e.g. '2' is actually '0002' *)
      val digits = Utils.pad 5 0 (Utils.digits code)
      val opc = Utils.fromDigits (List.drop (digits, length digits - 2))
      val modes = map toMode (List.take (digits, length digits - 2))
    in
      if List.exists (fn m => m = UNKNOWN) modes
      then (code, [UNKNOWN])
      else (opc, rev modes)
    end

  fun decode m ip : inst =
    tryRead (fn red =>
    let
      val (opc, modes) = opToInst red
    in
      if hd modes = UNKNOWN then UNKNOWN_MODE red
      else (fn 1 => createTernary ADD m ip modes
             | 2 => createTernary MULT m ip modes
             | 3 => createUnaryW INPUT m ip modes
             | 4 => createUnaryR OUTPUT m ip modes
             | 5 => createJmp (fn i => i <> 0) JMP_T m ip modes
             | 6 => createJmp (fn i => i = 0) JMP_F m ip modes
             | 7 => createTst (fn i => fn j => i < j) TST_LT m ip modes
             | 8 => createTst (fn i => fn j => i = j) TST_EQ m ip modes
             | 9 => createUnaryR SET_BASE m ip modes
             | 99 => HALT
             (* or should this throw? *)
             | u => UNKNOWN_OP u) opc
    end)
    (Memory.read m ip)
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

structure Decoder = IntDecoderFn (Memory)

structure StdIO : IO = struct
  type elem = int
  val reader =
    let val reader' = Option.valOf o TextIO.scanStream (Int.scan StringCvt.DEC)
    in fn () => reader' TextIO.stdIn
    end
  val writer = fn i => TextIO.print ((Int.toString i) ^ "\n")
end

structure Intcode = CPUFn (structure Memory = Memory
                           structure Decoder = Decoder)

structure Reader = struct
  fun read s : Intcode.program =
  let
    val collect = List.mapPartial Int.fromString
    fun sep c = Char.isSpace c orelse c = #","
    val tokenize = String.tokens sep
  in
    (Memory.base, (collect o tokenize) s)
  end
  val readFromStream : TextIO.instream -> Intcode.program =
    read o TextIO.inputAll
  val readFromFile : string -> Intcode.program =
    readFromStream o TextIO.openIn
end

(* useful interactively *)
structure I = struct
  fun interpret s = Intcode.interpret (Reader.read s) StdIO.reader StdIO.writer
end

structure Solution = struct
  fun solution prog = Intcode.interpret prog (fn () => 1) StdIO.writer
  val solve = solution o Reader.readFromFile
end
