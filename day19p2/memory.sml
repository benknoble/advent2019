signature MEMORY = sig
  type elem
  type addr
  type memory

  val base : addr

  val read : memory -> addr -> (addr, elem) Either'.either
  val write : memory -> addr -> elem -> (addr * elem, memory) Either'.either
  val tryRead : (addr -> 'a) -> (elem -> 'a)
                -> ((addr, elem) Either'.either -> 'a)
  val tryWrite : (addr * elem -> 'a) -> (memory -> 'a)
                 -> ((addr * elem, memory) Either'.either -> 'a)

  val getRelToBase : memory -> addr -> addr
  val setRelBase : memory -> addr -> memory

  val nextIP : int -> addr -> addr

  val nextN : int -> addr -> memory -> (addr, elem) Either'.either
  val next : addr -> memory -> (addr, elem) Either'.either
  val next2 : addr -> memory -> (addr, elem) Either'.either
  val next3 : addr -> memory -> (addr, elem) Either'.either

  val elemToAddr : elem -> addr
  val addrToElem : addr -> elem

  val add : elem * elem -> elem
  val mult : elem * elem -> elem
end

structure Memory : MEMORY = struct
  structure E = Either'
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
