val _ =
  OS.Process.exit
    (MLton.Pacml.run (MLton.Pacml.shutdown (Top.main
      (CommandLine.name (), CommandLine.arguments ())
    )))
