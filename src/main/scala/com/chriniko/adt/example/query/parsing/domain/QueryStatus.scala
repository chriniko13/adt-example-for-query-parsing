package com.chriniko.adt.example.query.parsing.domain

sealed trait QueryStatus {
  def v: String
}

case object PendingQueryStatus extends QueryStatus {
  override def v: String = "pending"
}

case object RunningQueryStatus extends QueryStatus {
  override def v: String = "running"
}

case object FinishedQueryStatus extends QueryStatus {
  override def v: String = "finished"
}

case object FailedQueryStatus extends QueryStatus {
  override def v: String = "failed"
}

case object UnknownQueryStatus extends QueryStatus {
  override def v: String = "unknown"
}


object QueryStatus {
  def apply(s: String): Option[QueryStatus] =
    s match {
      case "pending" => Some(PendingQueryStatus)
      case "running" => Some(RunningQueryStatus)
      case "finished" => Some(FinishedQueryStatus)
      case "failed" => Some(FailedQueryStatus)
      case "unknown" => Some(UnknownQueryStatus)
      case _ => None
    }
}