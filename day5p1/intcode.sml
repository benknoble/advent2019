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

structure Intcode = struct
  structure E = Either
  val L = E.L
  val R = E.R
  structure Memory = struct
    type elem = int
    type addr = int
    type memory = elem list
    type readSucc = elem
    type readErr = addr
    type writeSucc = memory
    type writeErr = addr * elem
    type readRes = (readErr, readSucc) E.either
    type writeRes = (writeErr, writeSucc) E.either

    fun read (m : memory) (p : addr) : readRes =
      R (List.nth (m, p))
      handle
      Subscript => L p
    fun read' (p : addr) (m : memory) : readRes = read m p

    fun write (m : memory) (p : addr) (e : elem) : writeRes =
      R (List.take(m, p) @ [e] @ List.drop(m, p+1))
      handle
      Subscript => L (p, e)
    fun write' (p : addr) (e : elem) (m : memory) : writeRes = write m p e

    val tryRead : (readErr -> 'a) -> (readSucc -> 'a) -> (readRes -> 'a) =
      E.lift
    val tryWrite : (writeErr -> 'a) -> (writeSucc -> 'a) -> (writeRes -> 'a) =
      E.lift

    fun nextN (n : int) (curr : addr) : memory -> readRes =
      read' (curr + n)
    val next = nextN 1
    val next2 = nextN 2
    val next3 = nextN 3
  end

  structure Decoder = struct
    type opcode = Memory.elem
    type arith_addrs = {
      srcA : Memory.addr
      , srcB : Memory.addr
      , dest : Memory.addr
      , newIp : Memory.addr
      }
    datatype inst = ADD of arith_addrs
                  | MULT of arith_addrs
                  | HALT
                  | UNKNOWN of opcode
                  | MEM_ERR of Memory.readErr

    val tryRead = Memory.tryRead MEM_ERR
    fun createArith
      (f : arith_addrs -> inst)
      (m : Memory.memory)
      (ip : Memory.addr)
      : inst =
      let val newIp = ip + 4
      in
        tryRead (fn a =>
        tryRead (fn b =>
        tryRead (fn d => f {srcA=a, srcB=b, dest=d, newIp=newIp})
        (Memory.next3 ip m))
        (Memory.next2 ip m))
        (Memory.next ip m)
      end
    fun decode (m : Memory.memory) (ip : Memory.addr) : inst =
      tryRead (fn 1 => createArith ADD m ip
                | 2 => createArith MULT m ip
                | 99 => HALT
                (* or should this throw? *)
                | u => UNKNOWN u)
      (Memory.read m ip)
  end

  structure CPU = struct
    datatype state = RUNNING
                   | MEM_R_ERR of Memory.readErr
                   | MEM_W_ERR of Memory.writeErr
                   | UNKNOWN_ERR of Memory.addr * Decoder.opcode
                   | FINISHED
    val init : Memory.addr = 0
    type program = Memory.memory
    type process = state * Memory.memory * Memory.addr
    type result = state * Memory.memory * Memory.addr

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
        ({srcA, srcB, dest, newIp} : Decoder.arith_addrs)
        : process =
        tryRead (fn a => fn (m', a') =>
        tryRead (fn b => fn (m'', a'') =>
        tryWrite (fn w => fn (m''', a''') =>
          (RUNNING, w, newIp))
        (Memory.write m dest (f (a,b))) (m'', a''))
        (Memory.read m srcB) (m', a'))
        (Memory.read m srcA) (m, ip)

      val add = arithOp (op +)
      val mult = arithOp (op *)
    end

    fun step (RUNNING, m, ip) = (
          case Decoder.decode m ip of
               Decoder.ADD a => Eval.add m ip a
             | Decoder.MULT a => Eval.mult m ip a
             | Decoder.HALT => (FINISHED, m, ip)
             | Decoder.UNKNOWN u => (UNKNOWN_ERR (ip, u), m, ip)
             | Decoder.MEM_ERR e => (MEM_R_ERR e, m, ip))
      (* all other states (finished, errors) are fixed points *)
      | step p = p

    fun load (p : program) : process = (RUNNING, p, init)
    fun run (s : state, m : Memory.memory, ip : Memory.addr) : result =
      if s = RUNNING
      then run (step (s, m, ip))
      else (s, m, ip)
    val interpret : program -> result = run o load
  end

  structure Reader = struct
    val read : string -> CPU.program =
    let
      val collect = List.mapPartial Int.fromString
      fun sep c = Char.isSpace c orelse c = #","
      val tokenize = String.tokens sep
    in
      collect o tokenize
    end
    val readFromStream : TextIO.instream -> CPU.program =
      read o TextIO.inputAll
    val readFromFile : string -> CPU.program =
      readFromStream o TextIO.openIn
  end
end
