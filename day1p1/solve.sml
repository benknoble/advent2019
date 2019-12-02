fun fuel mass = floor (mass / 3.0) - 2

val sum = foldl op + 0

val total_fuel = sum o (map fuel)

val read =
let
  val collect_reals = List.mapPartial Real.fromString
  val tokenize = String.tokens Char.isSpace
  val to_masses = collect_reals o tokenize
in
  to_masses o TextIO.inputAll
end

val solve = total_fuel o read o TextIO.openIn
