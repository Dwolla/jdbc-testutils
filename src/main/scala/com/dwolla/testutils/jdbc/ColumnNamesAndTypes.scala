package com.dwolla.testutils.jdbc

import shapeless.*

private[jdbc] sealed trait ColumnNamesAndTypes[T]
private[jdbc] object ColumnNamesAndTypes {
  implicit val stringIntTuple: ColumnNamesAndTypes[(String, Int)] = new ColumnNamesAndTypes[(String, Int)] {}
  implicit val hnil: ColumnNamesAndTypes[HNil] = new ColumnNamesAndTypes[HNil] {}
  implicit def hlist[H: ColumnNamesAndTypes, T <: HList : ColumnNamesAndTypes]: ColumnNamesAndTypes[H :: T] = new ColumnNamesAndTypes[H :: T] {}
}
