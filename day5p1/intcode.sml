(* a right-biased either type, based on scala.util.Either *)
structure Either = struct
  datatype ('a, 'b) either = L of 'a
                           | R of 'b
  fun fold (l : 'a -> 'c) (r : 'b -> 'c) (e : ('a, 'b) either) : 'c =
    case e of
         L a => l a
       | R b => r b
  (* fun map (f : 'b -> 'c) : ('a, 'b) either -> ('a, 'c) either = *)
  (*   fold (fn a => L a) (fn b => R (f b)) *)
  (* fun flatMap (f : 'b -> ('a, 'b) either) : ('a, 'b) either -> ('a, 'b) either = *)
  (*   fold (fn a => L a) (fn b => f b) *)
  (* fun contains (b : ''b) : ('a, ''b) either -> bool = *)
  (*   fold (fn _ => false) (fn b' => b = b') *)
  (* fun exists (p : 'b -> bool) : ('a, 'b) either -> bool = *)
  (*   fold (fn _ => false) (fn b => p b) *)
  (* fun filterOrElse (p : 'b -> bool) (zero : 'a) : *)
  (*   ('a, 'b) either -> ('a, 'b) either = *)
  (*   fold (fn a => L a) (fn b => if p b then R b else L zero) *)
  (* fun orElse (test : ('a, 'b) either) (or : ('a, 'b) either) : ('a, 'b) either = *)
  (*   fold (fn _ => or) (fn b => R b) test *)
  (* fun swap (e : ('a, 'b) either) : ('b, 'a) either = *)
  (*   fold (fn a => R a) (fn b => L b) e *)
  fun lift (f : 'a -> 'c) (g : 'b -> 'c): ('a, 'b) either -> 'c =
    fold f g
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
                -> ((addr * elem, memory) Either.either-> 'a)

  val nextIP : int -> addr -> addr
  val nextN : int -> addr -> memory -> (addr, elem) Either.either

  val next : addr -> memory -> (addr, elem) Either.either
  val next2 : addr -> memory -> (addr, elem) Either.either
  val next3 : addr -> memory -> (addr, elem) Either.either

  val elemToAddr : elem -> addr

  val add : elem * elem -> elem
  val mult : elem * elem -> elem
end

signature DECODER = sig
  type opcode
  type memory
  type addr

  type unary
  type ternary
  val unaryToRec : unary -> {addr : addr, newIp : addr}
  val ternaryToRec : ternary -> { srcA : addr
                                , srcB : addr
                                , dest : addr
                                , newIp : addr
                                }

  datatype inst = ADD of ternary
                | MULT of ternary
                | INPUT of unary
                | OUTPUT of unary
                | HALT
                | UNKNOWN of opcode
                | MEM_ERR of addr

  val decode : memory -> addr -> inst
end

signature CPU = sig
  type program
  type process
  type result
  val load : program -> process
  val run : process -> result
  (* usually just run o load *)
  val interpret : program -> result
end

structure Memory : MEMORY = struct
  structure E = Either
  val L = E.L
  val R = E.R

  type elem = int
  type addr = int
  type memory = elem list

  val base = 0

  type readSucc = elem
  type readErr = addr
  type writeSucc = memory
  type writeErr = addr * elem
  type readRes = (readErr, readSucc) E.either
  type writeRes = (writeErr, writeSucc) E.either

  fun read m p =
    R (List.nth (m, p))
    handle
    Subscript => L p
  fun read' p m = read m p

  fun write m p e =
    R (List.take(m, p) @ [e] @ List.drop(m, p+1))
    handle
    Subscript => L (p, e)
  fun write' p e m = write m p e

  val tryRead = E.lift
  val tryWrite = E.lift

  fun nextIP n ip = ip + n

  fun nextN n curr : memory -> readRes = read' (curr + n)
  val next = nextN 1
  val next2 = nextN 2
  val next3 = nextN 3

  fun elemToAddr e = e

  val add = op +
  val mult = op *
end

functor DecoderFn (Memory : MEMORY where type elem = int) : DECODER = struct
  type opcode = Memory.elem
  type memory = Memory.memory
  type addr = Memory.addr
  type unary = {
    addr : addr
    , newIp : addr
    }
  type ternary = {
    srcA : addr
    , srcB : addr
    , dest : addr
    , newIp : addr
    }
  datatype inst = ADD of ternary
                | MULT of ternary
                | INPUT of unary
                | OUTPUT of unary
                | HALT
                | UNKNOWN of opcode
                | MEM_ERR of Memory.addr

  val eta = Memory.elemToAddr
  val tryRead = Memory.tryRead MEM_ERR

  val unaryToRec = fn u => u
  val ternaryToRec = fn t => t

  fun createUnary
    (f : unary -> inst) (m : memory) (ip : addr) : inst =
    let val newIp = Memory.nextIP 2 ip
    in tryRead (fn a => f {addr=(eta a), newIp=newIp}) (Memory.next ip m)
    end
  fun createArith
    (f : ternary -> inst) (m : memory) (ip : addr)
    : inst =
    let val newIp = Memory.nextIP 4 ip
    in
      tryRead (fn a =>
      tryRead (fn b =>
      tryRead (fn d => f {srcA=(eta a), srcB=(eta b), dest=(eta d), newIp=newIp})
      (Memory.next3 ip m))
      (Memory.next2 ip m))
      (Memory.next ip m)
    end

  fun decode (m : memory) (ip : addr) : inst =
    tryRead (fn 1 => createArith ADD m ip
              | 2 => createArith MULT m ip
              | 3 => createUnary INPUT m ip
              | 4 => createUnary OUTPUT m ip
              | 99 => HALT
              (* or should this throw? *)
              | u => UNKNOWN u)
    (Memory.read m ip)
end

functor CPUFn (structure Memory : MEMORY
               structure Decoder : DECODER sharing Decoder = Memory
               structure IO : IO sharing IO = Memory) : CPU = struct
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
      tryWrite (fn w => fn (m''', a''') =>
        (RUNNING, w, newIp))
      (Memory.write m dest (f (a,b))) (m'', a''))
      (Memory.read m srcB) (m', a'))
      (Memory.read m srcA) (m, ip)
      end

    val add = arithOp Memory.add
    val mult = arithOp Memory.mult
    fun input
      (m : Memory.memory)
      (ip : Memory.addr)
      (u : Decoder.unary)
      : process =
      let val {addr, newIp} = Decoder.unaryToRec u
      in
      tryWrite (fn w => fn (m, a) => (RUNNING, w, newIp))
               (Memory.write m addr (IO.reader ())) (m, ip)
      end
    fun output
      (m : Memory.memory)
      (ip : Memory.addr)
      (u : Decoder.unary)
      : process =
      let val {addr, newIp} = Decoder.unaryToRec u
      in
      tryRead (fn e => fn (m, a) => (IO.writer e ; (RUNNING, m, newIp)))
              (Memory.read m addr) (m, ip)
      end
  end

  fun step (RUNNING, m, ip) = (
        case Decoder.decode m ip of
              Decoder.ADD a => Eval.add m ip a
            | Decoder.MULT a => Eval.mult m ip a
            | Decoder.INPUT a => Eval.input m ip a
            | Decoder.OUTPUT a => Eval.output m ip a
            | Decoder.HALT => (FINISHED, m, ip)
            | Decoder.UNKNOWN u => (UNKNOWN_ERR (ip, u), m, ip)
            | Decoder.MEM_ERR e => (MEM_R_ERR e, m, ip))
    (* all other states (finished, errors) are fixed points *)
    | step p = p

  fun load p = (RUNNING, p, init)
  fun run (s, m, ip) = case s of
                            RUNNING => run (step (s, m, ip))
                          | _ => (s, m, ip)
  val interpret = run o load
end

structure StdIO : IO = struct
  type elem = int
  val reader =
    let val reader' = Option.valOf o TextIO.scanStream (Int.scan StringCvt.DEC)
    in fn () => reader' TextIO.stdIn
    end
  val writer = fn i => TextIO.print ((Int.toString i) ^ "\n")
end

structure Intcode = CPUFn (structure Memory = Memory
                           structure Decoder = DecoderFn (Memory)
                           structure IO = StdIO)

structure Reader = struct
  val read : string -> Intcode.program =
  let
    val collect = List.mapPartial Int.fromString
    fun sep c = Char.isSpace c orelse c = #","
    val tokenize = String.tokens sep
  in
    collect o tokenize
  end
  val readFromStream : TextIO.instream -> Intcode.program =
    read o TextIO.inputAll
  val readFromFile : string -> Intcode.program =
    readFromStream o TextIO.openIn
end

val interpreter = Intcode.interpret o Reader.read
