package com.chriniko.adt.example.query.parsing.domain

import scala.collection.mutable.ListBuffer

case class QueryAttribute(name: String, columnName: String, values: ListBuffer[QueryAttributeValue] = ListBuffer()) {

  def +(queryAttributeValue: QueryAttributeValue): Unit = {
    values += queryAttributeValue
  }

}

object QueryAttribute {

  private val queryAttributeTypes: Map[String, Class[_]] = Map(
    "country" -> classOf[String],
    "model" -> classOf[String],
    "make" -> classOf[String],
    "os" -> classOf[String],
    "osv" -> classOf[String],
    "yob" -> classOf[Int],
    "gender" -> classOf[String],
    "language" -> classOf[String],
    "category" -> classOf[String],
    "bundle" -> classOf[String],
  )

  def getType(field: String): Class[_] = queryAttributeTypes.getOrElse(field, classOf[String])

  private val nameToColumnMappings = Map(
    "Gender" -> "gender",
    "Countries" -> "country",
    "Language" -> "language",
    "IAB category" -> "category",
    "OS" -> "os"
  )

  def apply(name: String): QueryAttribute = new QueryAttribute(name, nameToColumnMappings(name))

}

case class QueryAttributeValue(label: String, value: String)

object QueryAttributeValue {
  def apply(v: String): QueryAttributeValue = new QueryAttributeValue(v, v)
}
