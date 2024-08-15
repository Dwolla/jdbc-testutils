package com.dwolla.testutils.jdbc

import com.dwolla.test.*
import munit.{Location, ScalaCheckSuite, TestOptions}
import org.scalacheck.*
import smithy4s.Schema

class NullableSpec
  extends ScalaCheckSuite
    with Smithy4sArbitraries {

  testSchemaType[MySmithy4sNewtype]("Empty newtypes are mapped to null")
  testSchemaType[MyList]("Empty lists are mapped to null")
  testSchemaType[MyMap]("Empty maps are mapped to null")
  testSchemaType[MyStructure]("Empty structures are mapped to null")
  testSchemaType[MyUnion]("Empty unions are mapped to null")
  testSchemaType[MyEnum]("Empty enums are mapped to null")
  testSchemaType[MyRecursive]("Empty recursive structures are mapped to null")

  private implicit def optionSchema[A : Schema]: Schema[Option[A]] = Schema.option(Schema[A])

  private def testSchemaType[A : Arbitrary : Schema](testOptions: TestOptions)
                                                    (implicit loc: Location): Unit =
    test(testOptions) {
      Prop.forAllNoShrink { (maybeMaybeA: Option[Option[A]]) =>
        val optionOutput = Nullable.fromSchema[Option[A]].orNull(maybeMaybeA)
        val flattenedOutput = Nullable.fromSchema[A].orNull(maybeMaybeA.flatten)
        val expected = maybeMaybeA.flatten match {
          case Some(a) => a
          case None => null.asInstanceOf[A]
        }

        assertEquals(optionOutput: Any, expected)
        assertEquals(flattenedOutput: Any, expected)
      }
    }
}
