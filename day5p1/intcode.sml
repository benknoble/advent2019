structure Intcode = struct

  (* memory handling *)
  type elem = int
  type addr = int
  type memory = elem list
  exception MemOutOfBounds of addr

  fun read (m : memory) (p : addr) : elem =
    List.nth (m, p)
    handle
    Subscript => raise MemOutOfBounds p
  fun read' (p : addr) (m : memory) : elem = read m p

  fun write (m : memory) (p : addr) (e : elem) : memory =
    List.take(m, p) @ [e] @ List.drop(m, p+1)
    handle
    Subscript => raise MemOutOfBounds p
  fun write' (p : addr) (e : elem) (m : memory) : memory = write m p e

  (* decoder stage *)
  type opcode = elem
  type arith_addrs = {srcA : addr, srcB : addr, dest : addr, newIp : addr}
  datatype inst = ADD of arith_addrs
                | MULT of arith_addrs
                | HALT
                | UNKNOWN of opcode

  fun decode (m : memory) (ip : addr) : inst =
  let
    val next1 = read' (ip+1)
    val next2 = read' (ip+2)
    val next3 = read' (ip+3)
    val nextArith = ip+4
    val inst = read m ip
  in
    case inst of
         1 => ADD {
         srcA=(next1 m)
         , srcB=(next2 m)
         , dest=(next3 m)
         , newIp=nextArith
         }
       | 2 => MULT {
         srcA=(next1 m)
         , srcB=(next2 m)
         , dest=(next3 m)
         , newIp=nextArith
         }
       | 99 => HALT
       (* or should this throw? *)
       | u => UNKNOWN u
  end

  (* processor architecture *)
  datatype state = RUNNING
                 | FINISHED

  val init : addr = 0
  type program = memory
  type process = state * memory * addr
  type result = memory * addr

  (* evaluators *)
  fun arithOp
    (f : elem * elem -> elem)
    (m : memory)
    (ip : addr)
    {srcA : addr, srcB : addr, dest : addr, newIp : addr}
    : process =
  let
    val read = read m
    val write = write m
    val a = read srcA
    val b = read srcB
    val res = f (a,b)
  in
    (RUNNING, write dest res, newIp)
  end

  val add = arithOp (op +)
  val mult = arithOp (op *)

  (* single-step state-machine *)
  exception AlreadyDone
  exception UnknownOp of addr * opcode
  fun step (FINISHED, _, _) = raise AlreadyDone
    | step (RUNNING, m, ip) =
    case decode m ip of
         ADD a => add m ip a
       | MULT a => mult m ip a
       | HALT => (FINISHED, m, ip)
       | UNKNOWN u => raise UnknownOp (ip, u)

  (* main interface *)
  fun load (p : program) : process = (RUNNING, p, init)
  fun run (s : state, m : memory, ip : addr) : result =
    case s of
         RUNNING => run (step (s, m, ip))
       | FINISHED => (m, ip)

  val interpret : program -> result = run o load

  val readProgram : TextIO.instream -> program =
  let
    val collect = List.mapPartial Int.fromString
    fun sep c = Char.isSpace c orelse c = #","
    val tokenize = String.tokens sep
    val toList = collect o tokenize
  in
    toList o TextIO.inputAll
  end

  val readProgramFromFile : string -> program =
    readProgram o TextIO.openIn

end

fun fix noun verb =
let
  val write1 = Intcode.write' 1 noun
  val write2 = Intcode.write' 2 verb
in
  write2 o write1
end

fun run noun verb = Intcode.interpret o fix noun verb

fun searchSpace n =
let
  val nouns = List.tabulate (n, fn i => i)
  val verbs = nouns
  fun cartesian a b = List.concat (map (fn a => map (fn b => (a,b)) b) a)
in
  cartesian nouns verbs
end

fun output (m, ip) = Intcode.read m 0
fun check p noun verb target = output (run noun verb p) = target

fun first n p target =
let
  val search = searchSpace n
  fun go p nvs =
  let
    val (noun, verb) = hd nvs
  in
    if check p noun verb target then (noun, verb)
    else go p (tl nvs)
  end
in
  go p search
end

val target = 19690720
fun solve f =
let
  val prog = Intcode.readProgramFromFile f
  val bound = 99
  val (noun, verb) = first bound prog target
in
  100 * noun + verb
end
