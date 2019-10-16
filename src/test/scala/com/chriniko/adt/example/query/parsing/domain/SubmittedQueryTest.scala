package com.chriniko.adt.example.query.parsing.domain

import java.time.{LocalDateTime, ZoneOffset}

import org.scalatest.FunSuite
import spray.json.DefaultJsonProtocol._
import spray.json.{JsObject, JsString, _}

class SubmittedQueryTest extends FunSuite {


  test("json ser/de of submittedQuery works as expected") {

    // given
    val now = LocalDateTime.of(1992, 11, 17, 14, 0).toInstant(ZoneOffset.UTC)
    val q = SubmittedQuery(Id("1"), "sessionId", CountQueryType, "query")


    // when
    val serialized = q.toJson.toString()

    // then
    val expected = serializedQueryJson(q.createdAt.toString, q.updatedAt.toString)
    assert(expected == serialized)

    // when
    val jsValue = serialized.parseJson
    val deserialized = jsValue.convertTo[SubmittedQuery]

    // then
    assert(deserialized == q)
  }


  val serializedQueryJson: (String, String) => String = (created, updated) =>
    JsObject(
      "updatedAt" -> JsString(updated),
      "result" -> JsNull,
      "query" -> JsString("query"),
      "id"-> JsString("1"),
      "status" -> JsString("pending"),
      "sessionId" -> JsString("sessionId"),
      "createdAt" -> JsString(created),
      "type" -> JsString("count")
    ).toJson.toString()

}
