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

  test("isSessions / isCurrent: exact slash commands"):
    assert(CliCommands.isSessions("/sessions"))
    assert(CliCommands.isSessions("  /SESSIONS  "))
    assert(CliCommands.isCurrent("/current"))
    assert(!CliCommands.isSessions("sessions"))
    assert(!CliCommands.isCurrent("current"))

  test("commands do not collide"):
    assert(!CliCommands.isQuit("/sessions"))
    assert(!CliCommands.isSessions("/quit"))
    assert(!CliCommands.isCurrent("/sessions"))

  test("isSlashCommand: any leading slash"):
    assert(CliCommands.isSlashCommand("/anything"))
    assert(CliCommands.isSlashCommand("  /foo bar"))
    assert(!CliCommands.isSlashCommand("hello"))
    assert(!CliCommands.isSlashCommand(""))
