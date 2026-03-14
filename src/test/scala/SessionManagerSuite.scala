import tacit.executor.SessionManager
import tacit.core.{Context, Config}

class SessionManagerSuite extends munit.FunSuite:
  given Context = Context(Config(), None)

  // ── CRUD ───────────────────────────────────────────────────────────

  test("create and list sessions"):
    val manager = new SessionManager
    val sessionId = manager.createSession()
    assert(sessionId.nonEmpty)
    assert(manager.listSessions().contains(sessionId))

  test("delete session"):
    val manager = new SessionManager
    val sessionId = manager.createSession()
    assert(manager.deleteSession(sessionId))
    assert(!manager.listSessions().contains(sessionId))

  test("delete non-existent session"):
    val manager = new SessionManager
    assert(!manager.deleteSession("non-existent-id"))

  test("execute in session maintains state"):
    val manager = new SessionManager
    val sessionId = manager.createSession()

    // Define a variable
    val result1 = manager.executeInSession(sessionId, "val x = 42")
    assert(result1.isRight)

    // Use the variable in next execution
    val result2 = manager.executeInSession(sessionId, "x * 2")
    assert(result2.isRight)
    assert(result2.toOption.get.output.contains("84"))

  test("execute in non-existent session"):
    val manager = new SessionManager
    val result = manager.executeInSession("non-existent-id", "1 + 1")
    assert(result.isLeft)

  // ── Isolation and lifecycle ──────────────────────────────────────

  test("sessions are isolated from each other"):
    val manager = new SessionManager
    val s1 = manager.createSession()
    val s2 = manager.createSession()
    manager.executeInSession(s1, "val x = 1")
    manager.executeInSession(s2, "val x = 2")
    val r1 = manager.executeInSession(s1, "x")
    val r2 = manager.executeInSession(s2, "x")
    assert(r1.toOption.get.output.contains("1"))
    assert(r2.toOption.get.output.contains("2"))

  test("execute in deleted session returns Left"):
    val manager = new SessionManager
    val sessionId = manager.createSession()
    manager.deleteSession(sessionId)
    val result = manager.executeInSession(sessionId, "1 + 1")
    assert(result.isLeft)
    assert(result.left.exists(_.contains("not found")))

  test("re-delete already deleted session returns false"):
    val manager = new SessionManager
    val sessionId = manager.createSession()
    assert(manager.deleteSession(sessionId))
    assert(!manager.deleteSession(sessionId))

  test("multiple sessions listed correctly"):
    val manager = new SessionManager
    val ids = (1 to 3).map(_ => manager.createSession()).toSet
    val listed = manager.listSessions().toSet
    assertEquals(listed, ids)

  test("delete one session does not affect others"):
    val manager = new SessionManager
    val s1 = manager.createSession()
    val s2 = manager.createSession()
    manager.executeInSession(s1, "val a = 10")
    manager.executeInSession(s2, "val b = 20")
    manager.deleteSession(s1)
    val result = manager.executeInSession(s2, "b")
    assert(result.isRight)
    assert(result.toOption.get.output.contains("20"))
    assert(!manager.listSessions().contains(s1))
    assert(manager.listSessions().contains(s2))

  // ── State persistence ───────────────────────────────────────────

  test("session preserves imports across calls"):
    val manager = new SessionManager
    val sid = manager.createSession()
    manager.executeInSession(sid, "import java.time.LocalDate")
    val result = manager.executeInSession(sid, "LocalDate.of(2025, 1, 1).getYear")
    assert(result.isRight)
    assert(result.toOption.get.output.contains("2025"))

  test("session preserves function definitions"):
    val manager = new SessionManager
    val sid = manager.createSession()
    manager.executeInSession(sid, "def double(n: Int): Int = n * 2")
    val result = manager.executeInSession(sid, "double(21)")
    assert(result.isRight)
    assert(result.toOption.get.output.contains("42"))

  test("session handles compile error without corrupting state"):
    val manager = new SessionManager
    val sid = manager.createSession()
    val r1 = manager.executeInSession(sid, "val good = 42")
    assert(r1.toOption.get.success)
    val r2 = manager.executeInSession(sid, "val bad: Int = \"not an int\"")
    assert(!r2.toOption.get.success)
    val r3 = manager.executeInSession(sid, "good")
    assert(r3.toOption.get.success)
    assert(r3.toOption.get.output.contains("42"))
