package com.dwolla.testutils.jdbc

import com.dwolla.test.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

object Smithy4sArbitraries extends Smithy4sArbitraries

trait Smithy4sArbitraries {
  implicit val arbInteger: Arbitrary[Integer] = Arbitrary(arbitrary[Int].map[Integer](x => x))

  implicit val arbMySmithy4sNewtype: Arbitrary[MySmithy4sNewtype] =
    Arbitrary(arbitrary[String].map(MySmithy4sNewtype(_)))

  implicit val arbSmithy4sPrimitiveNewtype: Arbitrary[Smithy4sPrimitiveNewtype] =
    Arbitrary(arbitrary[Int].map(Smithy4sPrimitiveNewtype(_)))

  implicit val arbMyList: Arbitrary[MyList] =
    Arbitrary(arbitrary[List[String]].map(MyList(_)))

  implicit val arbMyMap: Arbitrary[MyMap] =
    Arbitrary(arbitrary[Map[String, Int]].map(MyMap(_)))

  implicit val arbMyStructure: Arbitrary[MyStructure] =
    Arbitrary {
      for {
        foo <- arbitrary[Option[String]]
        baz <- arbitrary[Int]
        maybeGreeting <- arbitrary[Option[String]]
      } yield maybeGreeting.fold(MyStructure(baz = baz, foo = foo)) { greeting =>
        MyStructure(baz = baz, greeting = greeting, foo = foo)
      }
    }

  implicit val arbMyUnion: Arbitrary[MyUnion] =
    Arbitrary(Gen.oneOf(arbitrary[Int].map(MyUnion.i32), arbitrary[String].map(MyUnion.string)))

  implicit val arbMyEnum: Arbitrary[MyEnum] =
    Arbitrary(Gen.oneOf(MyEnum.values))

  implicit def arbMyRecursive: Arbitrary[MyRecursive] =
    Arbitrary {
      for {
        foo <- arbitrary[Option[String]]
        recurse <- Gen.option(arbMyRecursive.arbitrary)
      } yield MyRecursive(foo, recurse)
    }
}
