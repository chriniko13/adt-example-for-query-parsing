package com.chriniko.adt.example.query.parsing.domain

import java.time.{Clock, Instant}

import spray.json.DefaultJsonProtocol._
import spray.json.{JsNull, JsObject, JsString, JsValue, RootJsonFormat, deserializationError}

case class SubmittedQuery(id: Id,
                          sessionId: String,
                          query: String,
                          var status: QueryStatus,
                          `type`: QueryType,
                          var result: Option[String],
                          createdAt: Instant,
                          var updatedAt: Instant) extends Serializable {

  import com.chriniko.adt.example.query.parsing.domain.SubmittedQuery._

  def error(error: String): SubmittedQuery = {
    status = FailedQueryStatus
    result = Some(error)
    updatedAt = getNow
    this
  }

  def update(status: QueryStatus): SubmittedQuery = {
    this.status = status
    updatedAt = getNow
    this
  }

  def complete(result: String): SubmittedQuery = {
    status = FinishedQueryStatus
    this.result = Some(result)
    updatedAt = getNow
    this
  }

}


object SubmittedQuery {

  private val clock: Clock = Clock.systemUTC()

  def apply(id: Id, sessionId: String, `type`: QueryType, query: String): SubmittedQuery = {
    val now = getNow
    new SubmittedQuery(id, sessionId, query, PendingQueryStatus, `type`, None, now, now)
  }

  def getNow: Instant = Instant.now(clock)


  // --- json capability ---
  implicit def submittedQueryJson: RootJsonFormat[SubmittedQuery] =
    new RootJsonFormat[SubmittedQuery] {

      override def read(json: JsValue): SubmittedQuery =

        json match {
          case JsObject(obj) =>
            SubmittedQuery(
              Id(obj("id").convertTo[String]),
              obj("sessionId").convertTo[String],
              obj("query").convertTo[String],
              QueryStatus(obj("status").convertTo[String]).getOrElse(UnknownQueryStatus),
              QueryType(obj("type").convertTo[String]).getOrElse(UnknownQueryType),
              extract(obj, "result"),
              Instant.parse(obj("createdAt").convertTo[String]),
              Instant.parse(obj("updatedAt").convertTo[String])
            )

          case _ =>
            deserializationError("not valid json for submitted query")

        }

      override def write(obj: SubmittedQuery): JsValue = {
        JsObject(
          "id" -> JsString(obj.id.value),
          "sessionId" -> JsString(obj.sessionId),
          "query" -> JsString(obj.query),
          "status" -> JsString(obj.status.v),
          "type" -> JsString(obj.`type`.v),
          "result" -> extract(obj.result),
          "createdAt" -> JsString(obj.createdAt.toString),
          "updatedAt" -> JsString(obj.updatedAt.toString)
        )
      }

      private def extract(m: Map[String, JsValue], v: String): Option[String] = if (m(v) == JsNull) None else Some(m(v).convertTo[String])

      private def extract(o: Option[String]): JsValue = if (o.isDefined) JsString(o.get) else JsNull
    }

}