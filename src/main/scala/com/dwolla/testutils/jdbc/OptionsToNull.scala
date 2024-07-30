package com.dwolla.testutils.jdbc

import org.typelevel.scalaccompat.annotation.unused
import shapeless.*

/**
 * Given an HList, strip any `Option[A]` values, replacing `None`s with `null`.
 * This replicates the behavior of JDBC, which uses `null` for missing values.
 */
private[jdbc] object OptionsToNull extends Poly1 {
  implicit def optionNullable[A >: Null]: Case.Aux[Option[A], A] = at[Option[A]](_.orNull)
  implicit def optionConvertableToNullable[A, B](implicit f: A => B,
                                                 ev: Null <:< B): Case.Aux[Option[A], B] = at[Option[A]](_.map(f).orNull)
  implicit def identityIfNotAnOption[A](implicit @unused ev: A <:!< Option[?]): Case.Aux[A, A] = at(x => x)
}
