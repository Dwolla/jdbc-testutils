package com.dwolla.testutils.jdbc

import com.dwolla.test.*
import munit.ScalaCheckSuite
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import shapeless.*

class OptionsToNullSpec
  extends ScalaCheckSuite
    with Smithy4sArbitraries {

  test("Empty options in HLists are mapped to null") {
    case class Smithy4sExamples(maybeSmithy4sPrimitiveNewtype: Option[Smithy4sPrimitiveNewtype],
                                maybeSmithy4sNewType: Option[MySmithy4sNewtype],
                                maybeList: Option[MyList],
                                maybeMap: Option[MyMap],
                                maybeStruct: Option[MyStructure],
                                maybeUnion: Option[MyUnion],
                                maybeEnum: Option[MyEnum],
                                maybeMyRecursive: Option[MyRecursive],
                               )
    object Smithy4sExamples {
      implicit val arbSmithy4sExamples: Arbitrary[Smithy4sExamples] = Arbitrary {
        for {
          maybeSmithy4sPrimitiveNewtype <- arbitrary[Option[Smithy4sPrimitiveNewtype]]
          maybeSmithy4sNewType <- arbitrary[Option[MySmithy4sNewtype]]
          maybeList <- arbitrary[Option[MyList]]
          maybeMap <- arbitrary[Option[MyMap]]
          maybeStruct <- arbitrary[Option[MyStructure]]
          maybeUnion <- arbitrary[Option[MyUnion]]
          maybeEnum <- arbitrary[Option[MyEnum]]
          maybeMyRecursive <- arbitrary[Option[MyRecursive]]
        } yield Smithy4sExamples(maybeSmithy4sPrimitiveNewtype, maybeSmithy4sNewType, maybeList, maybeMap, maybeStruct, maybeUnion, maybeEnum, maybeMyRecursive)
      }
    }

    Prop.forAllNoShrink { (maybeInt: Option[Int],
                           maybeString: Option[String],
                           maybeInteger: Option[Integer],
                           smithy4sExamples: Smithy4sExamples,
                           string: String,
                           boolean: Boolean,
                          ) =>
      val input =
        maybeInt ::
          smithy4sExamples.maybeSmithy4sPrimitiveNewtype ::
          smithy4sExamples.maybeSmithy4sNewType ::
          maybeString ::
          maybeInteger ::
          smithy4sExamples.maybeList ::
          smithy4sExamples.maybeMap ::
          smithy4sExamples.maybeStruct ::
          smithy4sExamples.maybeUnion ::
          smithy4sExamples.maybeEnum ::
          smithy4sExamples.maybeMyRecursive ::
          string ::
          boolean ::
          HNil

      val output = input.map(OptionsToNull)

      val expected =
        maybeInt.fold[Integer](null)(identity) ::
          smithy4sExamples.maybeSmithy4sPrimitiveNewtype.fold[Integer](null)(_.value) ::
          smithy4sExamples.maybeSmithy4sNewType.fold[String](null)(_.value) ::
          maybeString.fold[String](null)(identity) ::
          maybeInteger.fold[Integer](null)(identity) ::
          smithy4sExamples.maybeList.fold[List[String]](null)(_.value) ::
          smithy4sExamples.maybeMap.fold[Map[String, Int]](null)(_.value) ::
          smithy4sExamples.maybeStruct.fold[MyStructure](null)(identity) ::
          smithy4sExamples.maybeUnion.fold[MyUnion](null)(identity) ::
          smithy4sExamples.maybeEnum.fold[MyEnum](null)(identity) ::
          smithy4sExamples.maybeMyRecursive.fold[MyRecursive](null)(identity) ::
          string ::
          boolean ::
          HNil

      assertEquals(output: Any, expected)
    }
  }

}
