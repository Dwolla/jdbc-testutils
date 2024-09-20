package com.dwolla.testutils.jdbc

import com.dwolla.test.*
import munit.{Location, ScalaCheckSuite, TestOptions}
import org.scalacheck.*
import smithy4s.{Bijection, Newtype, Schema}

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

  testBijectionType(MySmithy4sNewtype)("Empty newtypes are mapped to null")
  testBijectionType(MyList)("Empty lists are mapped to null")
  testBijectionType(MyMap)("Empty maps are mapped to null")
  testNullableType[MyStructure]("Empty structures are mapped to null")
  testNullableType[MyUnion]("Empty unions are mapped to null")
  testNullableType[MyEnum]("Empty enums are mapped to null")
  testNullableType[MyRecursive]("Empty recursive structures are mapped to null")

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

  private def testBijectionType[B >: Null](obj: Newtype[B])
                                          (testOptions: TestOptions)
                                          (implicit B: Bijection[B, obj.Type],
                                           A: Arbitrary[obj.Type],
                                           loc: Location,
                                          ): Unit =
    test(testOptions) {
      Prop.forAllNoShrink { (maybeMaybeA: Option[Option[obj.Type]]) =>
        val flattenedOutput = Nullable.nullableFromBijection[B, obj.Type].orNull(maybeMaybeA.flatten)
        val expected = maybeMaybeA.flatten match {
          case Some(a) => a
          case None => null.asInstanceOf[obj.Type]
        }

        assertEquals(flattenedOutput: Any, expected)
      }
    }

  private def testNullableType[B >: Null](testOptions: TestOptions)
                                          (implicit A: Arbitrary[B],
                                           loc: Location,
                                          ): Unit =
    test(testOptions) {
      Prop.forAllNoShrink { (maybeMaybeA: Option[Option[B]]) =>
        val flattenedOutput = Nullable[B].orNull(maybeMaybeA.flatten)
        val expected = maybeMaybeA.flatten match {
          case Some(a) => a
          case None => null.asInstanceOf[B]
        }

        assertEquals(flattenedOutput: Any, expected)
      }
    }

}
