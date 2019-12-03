signature COMPUTER = sig
  type Elem
  type Pos
  type Memory

  val read : Memory -> Pos -> Elem
  val read' : Pos -> Memory -> Elem
  val write : Memory -> Pos -> Elem -> Memory
  val write' : Pos -> Elem -> Memory -> Memory

  val init : Pos
  type Program
  type Process
  type Result

  val compile : Program -> Process
  val step : Process -> Process
  val run : Process -> Result
  val interpret : Program ->  Result
end

structure Intcode : COMPUTER = struct
  type Elem = int
  type Pos = int
  type Memory = Elem list

  fun read m p = List.nth (m, p)
  fun read' p m = read m p
  fun write m p e = List.take(m, p) @ [e] @ List.drop(m, p+1)
  fun write' p e m = write m p e

  datatype State = RUNNING
                 | FINISHED

  val init = 0
  type Program = Memory
  type Process = State * Memory * Pos
  type Result = Memory * Pos

  (* parser stage *)
  datatype Opcode = ADD
                  | MULT
                  | HALT
                  | UNKNOWN of int
  fun parse (inst : int) : Opcode = case inst of
                                         1 => ADD
                                       | 2 => MULT
                                       | 99 => HALT
                                       (* or should this throw? *)
                                       | u => UNKNOWN u

  (* opcode implementations *)
  fun arith_op (f : int * int -> int) (m : Memory) (p : Pos) =
  let
    val read = read m
    val write = write m
    val src_a = read (p+1)
    val src_b = read (p+2)
    val dest = read (p+3)
    val a = read src_a
    val b = read src_b
    val res = f (a,b)
  in
    (RUNNING, write dest res, p+4)
  end

  val add = arith_op (op +)
  val mult = arith_op (op *)

  (* single-step state-machine *)
  exception AlreadyDone
  exception UnknownOp of int
  fun step (FINISHED, _, _) = raise AlreadyDone
    | step (RUNNING, m, p) =
    case parse (read m p) of
         ADD => add m p
       | MULT => mult m p
       | HALT => (FINISHED, m, p)
       | UNKNOWN u => raise UnknownOp u

  fun compile p = (RUNNING, p, init)
  fun run (s, m, p) = case s of
                           RUNNING => run (step (s, m, p))
                         | FINISHED => (m, p)
  val interpret = run o compile
end

val read_program =
let
  val collect = List.mapPartial Int.fromString
  fun sep c = Char.isSpace c orelse c = #","
  val tokenize = String.tokens sep
  val to_list = collect o tokenize
in
  to_list o TextIO.inputAll
end

val fix =
let
  val write1 = Intcode.write' 1 12
  val write2 = Intcode.write' 2 2
in
  write2 o write1
end

val run_input = Intcode.interpret o fix o read_program o TextIO.openIn

val solve = (Intcode.read' 0) o #1 o run_input
