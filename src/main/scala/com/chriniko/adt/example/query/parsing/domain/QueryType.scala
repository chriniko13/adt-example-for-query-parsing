package com.chriniko.adt.example.query.parsing.domain

sealed trait QueryType {
  def v: String
}

case object CountQueryType extends QueryType {
  override def v: String = "count"
}

case object MaidsQueryType extends QueryType {
  override def v: String = "maids"
}

case object UnknownQueryType extends QueryType {
  override def v: String = "unknown"
}

object QueryType {
  def apply(s: String) : Option[QueryType] =
    s match {
      case "count" => Some(CountQueryType)
      case "maids" => Some(MaidsQueryType)
      case "unknown" => Some(UnknownQueryType)
      case _ => None
    }
}
