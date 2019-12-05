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

fun elvesPasswdChecker (r : Range.range) : NumberPasswd.passwd -> bool =
  let
    open NumberPasswd
    val rules : rule list = [
      fn p => List.all (fn i => 0 <= i andalso i <= 9) p
     ,fn p => length p = 6
     ,fn p => let val p' = toInt p
              in #min r <= p' andalso p' <= #max r
              end
     ,fn p => List.exists (fn (a,b) => a=b) (ListPair.zip (p, tl p))
     ,fn p => #2 (foldl (fn (curr, (prev, notDecr)) =>
                           (curr, notDecr andalso curr >= prev))
                         (~1, true)
                         p)
                            ]
  in
    isValid rules
  end

fun check (isValid : NumberPasswd.passwd -> bool) : int -> bool option =
    (Option.map isValid) o NumberPasswd.fromInt

fun elvesCheck (r : Range.range) : int -> bool =
  elvesPasswdChecker r o valOf o NumberPasswd.fromInt

fun countValid (isValid : NumberPasswd.passwd -> bool) (r : Range.range) : int =
  let
    val passwds = map (valOf o NumberPasswd.fromInt) (Range.toList r)
  in
    length (List.filter isValid passwds)
  end

fun solution (r : string) : int option =
  let
    val ran = Range.fromString r
    val checker = Option.map elvesPasswdChecker ran
    val counter = Option.map countValid checker
  in
    Option.join (Option.map (fn r => Option.map (fn c => c r) counter) ran)
  end

val solve = solution o TextIO.inputAll o TextIO.openIn
