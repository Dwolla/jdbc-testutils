package com.dwolla.testutils.jdbc

import munit.ScalaCheckSuite
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import shapeless.*

class OptionsToNullSpec extends ScalaCheckSuite {
  private implicit val arbInteger: Arbitrary[Integer] = Arbitrary(arbitrary[Int].map[Integer](x => x))

  test("Empty options are mapped to null") {
    Prop.forAllNoShrink { (maybeInt: Option[Int],
                           maybeString: Option[String],
                           maybeInteger: Option[Integer],
                           string: String,
                           boolean: Boolean,
                          ) =>
      val input =
        maybeInt ::
          maybeString ::
          maybeInteger ::
          string ::
          boolean ::
          HNil

      val output = input.map(OptionsToNull)

      val expected =
        maybeInt.fold[Any](null)(identity) ::
          maybeString.fold[Any](null)(identity) ::
          maybeInteger.fold[Any](null)(identity) ::
          string ::
          boolean ::
          HNil

      assertEquals(output, expected)
    }
  }
}
