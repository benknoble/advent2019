fun fuel mass = floor(mass / 3.0) - 2

fun sum lst = foldl op+ 0 lst

fun read (file: TextIO.instream) =
let
  val collect_ints = List.mapPartial Int.fromString
  val tokenize = String.tokens Char.isSpace
  val to_masses = collect_ints o tokenize
in
  to_masses (TextIO.inputAll file)
end

fun main () =
let
  val masses = map real (read TextIO.stdIn)
  val fuels = map fuel masses
  val total = sum fuels
in
  print ((Int.toString total) ^ "\n") ;
  OS.Process.success
end

val _ = OS.Process.exit (main ())
