package com.dwolla.testutils.jdbc

import com.dwolla.test.*
import munit.ScalaCheckSuite
import shapeless.*

class FakeDatabaseSpec
  extends ScalaCheckSuite
    with Smithy4sArbitraries {

  test("FakeDatabase can use Smithy4s newtypes") {
    val input: Option[MySmithy4sNewtype] :: Option[MySmithy4sNewtype] :: Int :: Option[Int] :: HNil =
      Option(MySmithy4sNewtype("foo")) ::
        Option.empty[MySmithy4sNewtype] ::
        42 ::
        Option.empty[Int] ::
        HNil

    val rs = FakeDatabase.fakeResultSet(
      ("foo" -> 1) :: ("foo2" -> 1) :: ("foo3" -> 1)  :: ("foo" -> 1) :: HNil,
      input)

    rs.next()
    val output = (1 to 4).map(rs.getObject)

    assertEquals(output: Any, ("foo" :: null :: 42 :: null :: HNil).toList)
  }

}
