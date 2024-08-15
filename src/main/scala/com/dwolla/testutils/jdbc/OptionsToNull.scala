package com.dwolla.testutils.jdbc

import org.typelevel.scalaccompat.annotation.unused
import shapeless.*
import smithy4s.schema.*
import smithy4s.{Schema, *}

object OptionsToNull extends Poly1 {
  implicit def identityIfNotAnOption[A](implicit @unused ev: A <:!< Option[?]): Case[A] = at(identity)
  implicit def viaNullable[A](implicit ev: Nullable[A]): Case[Option[A]] = at(ev.orNull(_))
}

/**
 * Typeclass for converting a type `A` to a type `B` that is nullable.
 *
 * Scala types that extend AnyVal are non-nullable and therefore must
 * be converted to another type to hold a null value. Typically this
 * is not a problem in Scala because we use `Option` instead of null
 * values, but some APIs (notably, the JDBC API) use null for missing
 * values. This typeclass lets us generically convert a heterogeneous
 * list of values, some of which may be `None`, to values that can be
 * returned by the JDBC API.
 *
 * @tparam A The type of the value to be converted
 */
sealed trait Nullable[A] { outer =>
  type B // must be >: Null, but we can't enforce that or some of the instances below will fail
  val f: A => B

  final def orNull(maybeA: Option[A]): B =
    maybeA match {
      case Some(a) => f(a)
      case None =>
        /* the argument for this cast being ok is that Nullable is
           * sealed and all the instances that have been defined have
           * B >: Null. Unfortunately we can't prove that to the
           * compiler because the Smithy4s schema types don't have
           * that constraint.
           */
        null.asInstanceOf[B] // TODO suspicious cast
    }

  final def contramap[T](cmf: T => A): Nullable.Aux[T, B] = new Nullable[T] {
    override type B = outer.B
    override val f: T => B = outer.f.compose(cmf)
  }
}

object Nullable extends Smithy4sNullableInstances {
  type Aux[A, B1] = Nullable[A] { type B = B1 }
  def apply[A](implicit ev: Nullable[A]): ev.type = ev

  implicit val nullableChar: Nullable.Aux[Char, java.lang.Character] = makeConversion(char2Character)
  implicit val nullableShort: Nullable.Aux[Short, java.lang.Short] = makeConversion(short2Short)
  implicit val nullableInt: Nullable.Aux[Int, java.lang.Integer] = makeConversion(int2Integer)
  implicit val nullableFloat: Nullable.Aux[Float, java.lang.Float] = makeConversion(float2Float)
  implicit val nullableLong: Nullable.Aux[Long, java.lang.Long] = makeConversion(long2Long)
  implicit val nullableDouble: Nullable.Aux[Double, java.lang.Double] = makeConversion(double2Double)
  implicit val nullableBoolean: Nullable.Aux[Boolean, java.lang.Boolean] = makeConversion(boolean2Boolean)
  implicit val nullableByte: Nullable.Aux[Byte, java.lang.Byte] = makeConversion(byte2Byte)

  implicit def nullableAnyRef[A >: Null]: Nullable.Aux[A, A] = makeIdentity

  private[jdbc] def makeIdentity[A]: Nullable.Aux[A, A] = new Nullable[A] {
    override type B = A
    override val f: A => B = identity
  }

  private[jdbc] def makeConversion[A, B1](f1: A => B1): Nullable.Aux[A, B1] = new Nullable[A] {
    override type B = B1
    override val f: A => B = f1
  }
}

trait Smithy4sNullableInstances {
  implicit def fromSchema[A : Schema]: Nullable[A] =
    SchemaVisitorNullable.fromSchema(Schema[A], CompilationCache.make[Nullable])
}

private object SchemaVisitorNullable extends CachedSchemaCompiler.Impl[Nullable] {
  override protected type Aux[A] = Nullable[A]

  override def fromSchema[A](schema: Schema[A], cache: CompilationCache[Nullable]): Nullable[A] =
    schema.compile(new SchemaVisitorNullable(cache))
}

private class SchemaVisitorNullable(override protected val cache: CompilationCache[Nullable]) extends SchemaVisitor.Cached[Nullable] { self =>
  override def primitive[P](shapeId: ShapeId, hints: Hints, tag: Primitive[P]): Nullable[P] =
    Primitive.deriving[Nullable].apply(tag)

  override def collection[C[_], A](shapeId: ShapeId,
                                   hints: Hints,
                                   tag: CollectionTag[C],
                                   member: Schema[A]): Nullable.Aux[C[A], C[A]] =
    Nullable.makeIdentity

  override def map[K, V](shapeId: ShapeId,
                         hints: Hints,
                         key: Schema[K],
                         value: Schema[V]): Nullable.Aux[Map[K, V], Map[K, V]] =
    Nullable.makeIdentity

  override def enumeration[E](shapeId: ShapeId,
                              hints: Hints,
                              tag: EnumTag[E],
                              values: List[EnumValue[E]],
                              total: E => EnumValue[E]): Nullable.Aux[E, E] =
    Nullable.makeIdentity

  override def struct[S](shapeId: ShapeId,
                         hints: Hints,
                         fields: Vector[Field[S, ?]],
                         make: IndexedSeq[Any] => S): Nullable.Aux[S, S] =
    Nullable.makeIdentity

  override def union[U](shapeId: ShapeId,
                        hints: Hints,
                        alternatives: Vector[Alt[U, ?]],
                        dispatch: Alt.Dispatcher[U]): Nullable.Aux[U, U] =
    Nullable.makeIdentity

  override def biject[A, B](schema: Schema[A],
                            bijection: Bijection[A, B]): Nullable[B] =
    self(schema).contramap(bijection.from)

  override def refine[A, B](schema: Schema[A],
                            refinement: Refinement[A, B]): Nullable[B] =
    self(schema).contramap(refinement.from)

  override def lazily[A](suspend: smithy4s.Lazy[Schema[A]]): Nullable[A] =
    suspend.map(self(_)).value

  /**
   * `Nullable[Option[A]]` means [[Nullable.orNull]] will receive
   * an `Option[Option[A]]` which this instance effectively flattens,
   * so that it returns `null` if either the inner or outer `Option`
   * is empty and returns the innermost value if both are `Some`s.
   */
  override def option[A](schema: Schema[A]): Nullable[Option[A]] = {
    val c: Nullable[A] = self(schema) // precompile the schema to Nullable[A]

    new Nullable[Option[A]] {
      override type B = c.B
      override val f: Option[A] => B = c.orNull
    }
  }
}
