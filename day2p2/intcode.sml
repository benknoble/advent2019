structure Intcode = struct

  (* memory handling *)
  type Elem = int
  type Addr = int
  type Memory = Elem list
  exception MemOutOfBounds of Addr

  fun read (m : Memory) (p : Addr) : Elem =
    List.nth (m, p)
    handle
    Subscript => raise MemOutOfBounds p
  fun read' (p : Addr) (m : Memory) : Elem = read m p

  fun write (m : Memory) (p : Addr) (e : Elem) : Memory =
    List.take(m, p) @ [e] @ List.drop(m, p+1)
    handle
    Subscript => raise MemOutOfBounds p
  fun write' (p : Addr) (e : Elem) (m : Memory) : Memory = write m p e

  (* decoder stage *)
  type Opcode = Elem
  type ArithAddrs = {src_a : Addr, src_b : Addr, dest : Addr, new_ip : Addr}
  datatype Inst = ADD of ArithAddrs
                | MULT of ArithAddrs
                | HALT
                | UNKNOWN of Opcode

  fun decode (m : Memory) (ip : Addr) : Inst =
  let
    val next1 = read' (ip+1)
    val next2 = read' (ip+2)
    val next3 = read' (ip+3)
    val next_arith = ip+4
    val inst = read m ip
  in
    case inst of
         1 => ADD {
         src_a=(next1 m)
         , src_b=(next2 m)
         , dest=(next3 m)
         , new_ip=next_arith
         }
       | 2 => MULT {
         src_a=(next1 m)
         , src_b=(next2 m)
         , dest=(next3 m)
         , new_ip=next_arith
         }
       | 99 => HALT
       (* or should this throw? *)
       | u => UNKNOWN u
  end

  (* processor architecture *)
  datatype State = RUNNING
                 | FINISHED

  val init : Addr = 0
  type Program = Memory
  type Process = State * Memory * Addr
  type Result = Memory * Addr

  (* evaluators *)
  fun arith_op
    (f : Elem * Elem -> Elem)
    (m : Memory)
    (ip : Addr)
    {src_a : Addr, src_b : Addr, dest : Addr, new_ip : Addr}
    : Process =
  let
    val read = read m
    val write = write m
    val a = read src_a
    val b = read src_b
    val res = f (a,b)
  in
    (RUNNING, write dest res, new_ip)
  end

  val add = arith_op (op +)
  val mult = arith_op (op *)

  (* single-step state-machine *)
  exception AlreadyDone
  exception UnknownOp of Addr * Opcode
  fun step (FINISHED, _, _) = raise AlreadyDone
    | step (RUNNING, m, ip) =
    case decode m ip of
         ADD a => add m ip a
       | MULT a => mult m ip a
       | HALT => (FINISHED, m, ip)
       | UNKNOWN u => raise UnknownOp (ip, u)

  (* main interface *)
  fun compile (p : Program) : Process = (RUNNING, p, init)
  fun run (s : State, m : Memory, ip : Addr) : Result =
    case s of
         RUNNING => run (step (s, m, ip))
       | FINISHED => (m, ip)

  val interpret : Program -> Result = run o compile

  val read_program : TextIO.instream -> Program =
  let
    val collect = List.mapPartial Int.fromString
    fun sep c = Char.isSpace c orelse c = #","
    val tokenize = String.tokens sep
    val to_list = collect o tokenize
  in
    to_list o TextIO.inputAll
  end

  val read_program_from_file : string -> Program =
    read_program o TextIO.openIn

end

fun fix noun verb =
let
  val write1 = Intcode.write' 1 noun
  val write2 = Intcode.write' 2 verb
in
  write2 o write1
end

fun run noun verb = Intcode.interpret o fix noun verb

fun search_space n =
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
  val search = search_space n
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
  val prog = Intcode.read_program_from_file f
  val bound = 99
  val (noun, verb) = first bound prog target
in
  100 * noun + verb
end
