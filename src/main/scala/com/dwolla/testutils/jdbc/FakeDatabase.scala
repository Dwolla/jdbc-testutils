package com.dwolla.testutils.jdbc

import org.h2.tools.SimpleResultSet
import org.typelevel.scalaccompat.annotation.unused
import shapeless.*
import shapeless.ops.hlist
import shapeless.ops.hlist.{Mapper, ToTraversable}

import java.lang
import java.sql.*
import java.time.*
import scala.annotation.implicitNotFound
import scala.util.Try

object FakeDatabase {
  /**
   * Returns a fake `jdbc.sql.Connection` that will create `java.sql.PreparedStatement`s that return
   * `java.sql.ResultSet`s containing the passed rows. The column names and types will be derived
   * from the values specified in the `columns` parameter.
 *
   * @param columns An `HList` containing `(String, Int)` tuples mapping column names as strings to JDBC types as ints. This is defined as an `HList` because we need to statically know the length of the list.
   * @param rows A vararg of `HList`s containing the values to be returned. The number of columns in each `HList` must match the number of columns defined in the `columns` argument.
   * @return a fake `jdbc.sql.Connection` that will create `java.sql.PreparedStatement`s that return `java.sql.ResultSet`s containing the passed rows
   */
  def fakeConnection[Columns <: HList, Row <: HList, S <: Nat, NullableRow <: HList](columns: Columns,
                                                                                     rows: Row*)
                                                                                    (implicit
                                                                                     @implicitNotFound("the `columns` argument must be an HList containing `(String, Int)` tuples mapping column names as strings to JDBC types as ints")
                                                                                     ev0: ColumnNamesAndTypes[Columns],
                                                                                     ev1: ToTraversable.Aux[Columns, List, (String, Int)],
                                                                                     m: Mapper.Aux[OptionsToNull.type, Row, NullableRow],
                                                                                     ev2: ToTraversable.Aux[NullableRow, List, AnyRef],
                                                                                     s1: hlist.Length.Aux[Columns, S],
                                                                                     @implicitNotFound("make sure the number of columns matches the number of values in each row")
                                                                                     s2: hlist.Length.Aux[NullableRow, S],
                                                                                    ): Connection = {
    val stmt: PreparedStatement = new FakePreparedStatement {
      override def executeQuery(): ResultSet = fakeResultSet(columns, rows *)
      override def executeBatch(): scala.Array[Int] = scala.Array.fill(rows.size)(1)
    }

    new UnimplementedConnection {
      override def prepareStatement(sql: String): PreparedStatement = stmt
      override def createStatement(): Statement = new FakePreparedStatement
    }
  }

  /**
   * Returns a fake `java.sql.ResultSet`s containing the passed rows. The column names and types
   * will be derived from the values specified in the `columns` parameter.
   *
   * @param columns An `HList` containing `(String, Int)` tuples mapping column names as strings to JDBC types as ints. This is defined as an `HList` because we need to statically know the length of the list.
   * @param rows A vararg of `HList`s containing the values to be returned. The number of columns in each `HList` must match the number of columns defined in the `columns` argument.
   * @return a fake `jdbc.sql.Connection` that will create `java.sql.PreparedStatement`s that return `java.sql.ResultSet`s containing the passed rows
   */
  def fakeResultSet[Columns <: HList, Row <: HList, S <: Nat, NullableRow <: HList](columns: Columns,
                                                                                    rows: Row*)
                                                                                   (implicit
                                                                                    @implicitNotFound("the `columns` argument must be an HList containing `(String, Int)` tuples mapping column names as strings to JDBC types as ints")
                                                                                    @unused ev0: ColumnNamesAndTypes[Columns],
                                                                                    ev1: ToTraversable.Aux[Columns, List, (String, Int)],
                                                                                    m: Mapper.Aux[OptionsToNull.type, Row, NullableRow],
                                                                                    ev2: ToTraversable.Aux[NullableRow, List, AnyRef],
                                                                                    @unused s1: hlist.Length.Aux[Columns, S],
                                                                                    @implicitNotFound("make sure the number of columns matches the number of values in each row")
                                                                                    @unused s2: hlist.Length.Aux[NullableRow, S],
                                                                                   ): ResultSet = {
    val rs: SimpleResultSet = new SimpleResultSet() {
      override def getObject[T](columnIndex: Int, `type`: Class[T]): T = {
        if (`type` == classOf[LocalDate]) {
          val value = getObject(columnIndex)

          (value match {
            case x if x == null => null.asInstanceOf[T]
            case ld: LocalDate => ld
            case ldt: LocalDateTime => ldt.toLocalDate
            case l: lang.Long => Instant.ofEpochMilli(l).atZone(ZoneId.systemDefault()).toLocalDate
            case _ => throw new IllegalArgumentException(s"could not convert $value to ${`type`}")
          }).asInstanceOf[T]
        } else if (`type` == classOf[LocalDateTime]) {
          val value = getObject(columnIndex)

          (value match {
            case x if x == null => null.asInstanceOf[T]
            case ld: LocalDate => ld.atStartOfDay()
            case ldt: LocalDateTime => ldt
            case l: lang.Long => Instant.ofEpochMilli(l).atZone(ZoneId.systemDefault()).toLocalDateTime
            case _ => throw new IllegalArgumentException(s"could not convert $value to ${`type`}")
          }).asInstanceOf[T]
        } else
          super.getObject(columnIndex, `type`)
      }
    }

    for ((name, tpe) <- columns.toList) rs.addColumn(name, tpe, Integer.MAX_VALUE, 0)
    for (row <- rows) rs.addRow(row.map(OptionsToNull).toList *)

    rs
  }

  def parseAsDateInEpochMillis(s: String): Option[java.lang.Long] =
    for {
      nonNullS <- Option(s)
      if nonNullS.nonEmpty
      ld <- Try(LocalDate.parse(nonNullS)).toOption
      l <- parseAsDateInEpochMillis(ld)
    } yield l

  def parseAsDateInEpochMillis(s: LocalDate): Option[java.lang.Long] =
    parseAsDateInEpochMillis(s.atStartOfDay())

  def parseAsDateInEpochMillis(s: LocalDateTime): Option[java.lang.Long] =
    parseAsDateInEpochMillis(s.atZone(ZoneId.systemDefault()))

  def parseAsDateInEpochMillis(s: ZonedDateTime): Option[java.lang.Long] =
    Try(java.lang.Long.valueOf(s.toEpochSecond * 1000)).toOption
}
