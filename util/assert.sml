structure Assert =
struct
  fun say s = TextIO.output (TextIO.stdErr, s ^ "\n")
  fun say_nonewline s = TextIO.output (TextIO.stdErr, s)

  fun say_yellow s = say ("\027[01;33m" ^ s ^ "\027[00m")
  fun say_green  s = say ("\027[01;32m" ^ s ^ "\027[00m")
  fun say_red    s = say ("\027[01;31m" ^ s ^ "\027[00m")

  fun assert msg true = true
    | assert msg false = (say_red ("Assertion failed: " ^ msg); false)

  fun newline () = TextIO.output (TextIO.stdErr, "\n")
end
