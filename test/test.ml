let () =
  Alcotest.run "mao-lang"
    [
      ("Utils", TestUtils.tests);
      ("Loc", TestLoc.tests);
      ("ReaderStateResultMonad", TestReaderStateResultMonad.tests);
      ("DataStructure", TestDataStructure.tests);
      ("Lexer", TestLexer.tests);
      ("Parser", TestParser.tests);
      ("TypeChecker", TestTypeChecker.tests);
      ("Interpreter", TestInterpreter.tests);
      ("Trace", TestTrace.tests);
      ("MaoLang", TestMaoLang.tests);
    ]
