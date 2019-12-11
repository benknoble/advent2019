signature DECODER = sig
  type opcode
  type elem
  type memory
  type addr

  datatype mode = POS
                | IMM
                | REL
                | UNKNOWN

  type param = addr * mode * (unit -> (addr, elem) Either'.either)
  type dest = addr * mode * (elem -> (addr * elem, memory) Either'.either)

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

functor IntDecoderFn (Memory : MEMORY where type elem = int) : DECODER = struct
  type opcode = Memory.elem
  type elem = Memory.elem
  type memory = Memory.memory
  type addr = Memory.addr

  datatype mode = POS
                | IMM
                | REL
                | UNKNOWN
  type param = addr * mode * (unit -> (addr, elem) Either'.either)
  type dest = addr * mode * (elem -> (addr * elem, memory) Either'.either)

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
        | IMM => (fn () => Either'.R (Memory.addrToElem a))
        | REL => (fn () => Memory.read mem (Memory.getRelToBase mem a))
        | UNKNOWN => (fn () => Either'.L a))

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
        (*                     (fn e' => Either'.L (e', e)) *)
        (*                     (fn a' => Memory.write mem (eta a') e) *)
        (*                     (Memory.read mem a)) *)
        | REL => Memory.write mem (Memory.getRelToBase mem a)
        | UNKNOWN => (fn e => Either'.L (a, e)))

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
