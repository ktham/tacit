package tacit.library

class CommandValidatorSuite extends munit.FunSuite:

  test("validate allows command in allowed set"):
    // Should not throw
    CommandValidator.validate("echo", ProcessPermission(Set("echo")))

  test("validate rejects command not in allowed set"):
    val ex = intercept[SecurityException]:
      CommandValidator.validate("rm", ProcessPermission(Set("echo")))
    assert(ex.getMessage.nn.contains("Access denied"))
    assert(ex.getMessage.nn.contains("rm"))

  test("validate blocks cat in strict mode"):
    val ex = intercept[SecurityException]:
      CommandValidator.validate("cat", ProcessPermission(Set("cat"), strictMode = true))
    assert(ex.getMessage.nn.contains("Strict mode"))

  test("validate blocks ls in strict mode"):
    val ex = intercept[SecurityException]:
      CommandValidator.validate("ls", ProcessPermission(Set("ls"), strictMode = true))
    assert(ex.getMessage.nn.contains("Strict mode"))

  test("validate blocks rm in strict mode"):
    val ex = intercept[SecurityException]:
      CommandValidator.validate("rm", ProcessPermission(Set("rm"), strictMode = true))
    assert(ex.getMessage.nn.contains("Strict mode"))

  test("validate blocks cp in strict mode"):
    val ex = intercept[SecurityException]:
      CommandValidator.validate("cp", ProcessPermission(Set("cp"), strictMode = true))
    assert(ex.getMessage.nn.contains("Strict mode"))

  test("validate allows cat in non-strict mode"):
    // Should not throw
    CommandValidator.validate("cat", ProcessPermission(Set("cat"), strictMode = false))

  test("validate allows non-file command in strict mode"):
    // echo is not a file operation command
    CommandValidator.validate("echo", ProcessPermission(Set("echo"), strictMode = true))

  test("validate blocks tar in strict mode"):
    val ex = intercept[SecurityException]:
      CommandValidator.validate("tar", ProcessPermission(Set("tar"), strictMode = true))
    assert(ex.getMessage.nn.contains("Strict mode"))

  test("validate blocks chmod in strict mode"):
    val ex = intercept[SecurityException]:
      CommandValidator.validate("chmod", ProcessPermission(Set("chmod"), strictMode = true))
    assert(ex.getMessage.nn.contains("Strict mode"))

  test("validate rejects empty command not in allowed set"):
    val ex = intercept[SecurityException]:
      CommandValidator.validate("", ProcessPermission(Set("echo")))
    assert(ex.getMessage.nn.contains("Access denied"))
