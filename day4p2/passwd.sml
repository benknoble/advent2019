signature PASSWD = sig
  type passwd
  type rule = passwd -> bool
  val isValid : rule list -> passwd -> bool
  val fromInt : int -> passwd option
  val toInt : passwd -> int
end

structure NumberPasswd : PASSWD = struct
  type passwd = int list
  type rule = passwd -> bool
  val isValid = fn rules => fn p => List.all (fn r => r p) rules
  fun fromInt i =
    let
      val explode =
        map (valOf o Int.fromString o Char.toString)
        o String.explode
        o Int.toString
    in
      SOME (explode i)
    end
  val toInt : passwd -> int = foldl (fn (i, r) => 10*r + i) 0
end

structure Range = struct
  type range = {min : int, max : int}
  fun fromString (s : string) : range option =
    let
      val tokens = String.tokens (fn c => c = #"-") s
      val ints = map Int.fromString tokens
    in
      if length ints < 2 then NONE
      else case ints of
                (SOME min) :: (SOME max) :: rest => SOME {min=min, max=max}
              | _ => NONE
    end
  fun toList (r : range) : int list =
    List.drop (List.tabulate (#max r + 1, (fn i => i)), #min r)
end

fun windows (n : int) (xs : 'a list) : 'a list list =
  if n = length xs then [xs]
  else if n <= 0 orelse null xs then []
  else if n = 1 then map (fn x => [x]) xs
  else
    let
      val prev = windows (n-1) (xs)
      fun merge (xs, ys) = xs @ (List.drop (ys, n-2))
    in
      map merge (ListPair.zip (prev, (tl prev)))
    end

fun pairs p : (int option * (int * int) * int option) list =
  let
    val blocks = windows 4 p
    val blocks3 = windows 3 p
    val fst = hd blocks3
    val lst = List.last blocks3
  in
    ((fn [a,b,c] => (SOME a, (b,c), NONE)) fst)
    ::
    ((fn [b,c,d] => (NONE, (b,c), SOME d)) lst)
    ::
    (map (fn [a,b,c,d] => (SOME a, (b,c), SOME d)) blocks)
  end

fun elvesPasswdChecker (r : Range.range) : NumberPasswd.rule =
  let
    open NumberPasswd
    val rules : rule list = [
      (* digits *)
      fn p => List.all (fn i => 0 <= i andalso i <= 9) p
     ,fn p => length p = 6
      (* range validation *)
     ,fn p => let val p' = toInt p
              in #min r <= p' andalso p' <= #max r
              end
      (* there is a pair that is not in a group *)
     ,fn p =>
        List.exists (fn (a,(b,c),d) =>
          case (a, d) of
               (NONE, NONE) => b = c
             | (SOME a', NONE) => a' = b andalso b <> c
             | (NONE, SOME d') => b <> c andalso c = d'
             | (SOME a', SOME d') => a' <> b andalso b = c andalso c <> d')
          (pairs p)
      (* not decreasing *)
     ,fn p => #2 (foldl (fn (curr, (prev, notDecr)) =>
                           (curr, notDecr andalso curr >= prev))
                         (~1, true)
                         p)
                            ]
  in
    isValid rules
  end

(* useful for checking single values in testing *)
fun check (isValid : NumberPasswd.rule) : int -> bool option =
  (Option.map isValid) o NumberPasswd.fromInt
val elvesCheck : Range.range -> int -> bool option =
  check o elvesPasswdChecker

fun countValid (isValid : NumberPasswd.rule) : Range.range -> int =
  let val passwds = map (valOf o NumberPasswd.fromInt) o Range.toList
  in length o List.filter isValid o passwds
  end

fun solution (rangeS : string) : int option =
  let
    val ran = Range.fromString rangeS
    val counter = countValid o elvesPasswdChecker
  in
    Option.map (fn r => counter r r) ran
  end

val solve = solution o TextIO.inputAll o TextIO.openIn
