package capybaraclaw.gateway.port.cli

import munit.FunSuite

class CliCommandsSuite extends FunSuite:

  test("isQuit: recognized literals"):
    assert(CliCommands.isQuit("quit"))
    assert(CliCommands.isQuit("/quit"))
    assert(CliCommands.isQuit("exit"))
    assert(CliCommands.isQuit("/exit"))

  test("isQuit: case-insensitive and whitespace-tolerant"):
    assert(CliCommands.isQuit("QUIT"))
    assert(CliCommands.isQuit("Exit"))
    assert(CliCommands.isQuit("  quit  "))
    assert(CliCommands.isQuit("\t/exit\n"))

  test("isQuit: not a quit"):
    assert(!CliCommands.isQuit(""))
    assert(!CliCommands.isQuit("hello"))
    assert(!CliCommands.isQuit("quitting"))
    assert(!CliCommands.isQuit("q"))
    assert(!CliCommands.isQuit("quit now"))
