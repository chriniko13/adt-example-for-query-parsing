package com.chriniko.adt.example.query.parsing.dto

import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.util.{Failure, Success, Try}


/*
   Note: Recursive ADT

   TreeFormatRule = TreeFormatNestedRule(id * frequency * combinator * [TreeFormatRule] * not) + TreeFormatFlatRule(id * field * operator * value)

 */

sealed trait TreeFormatRule {
  def id: String
}

final case class TreeFormatFlatRule(id: String, field: String, operator: String, value: TreeFormatFlatRuleValue) extends TreeFormatRule

final case class TreeFormatNestedRule(id: String, frequency: Option[Int], combinator: String, rules: List[TreeFormatRule], not: Boolean) extends TreeFormatRule

object TreeFormatRule {

  import TreeFormatRuleObject._

  def apply(s: String): TreeFormatNestedRule = {
    s.parseJson.convertTo[TreeFormatNestedRule]
  }

}

/*
  Note: ADT

  TreeFormatFlatRuleValue = TreeFormatFlatRuleSingleValue(value) + TreeFormatFlatRuleMultiValue([value])

 */
sealed trait TreeFormatFlatRuleValue

final case class TreeFormatFlatRuleSingleValue(value: String) extends TreeFormatFlatRuleValue

final case class TreeFormatFlatRuleMultiValue(value: List[String]) extends TreeFormatFlatRuleValue

object TreeFormatFlatRuleMultiValue {
  def apply(elems: String*): TreeFormatFlatRuleMultiValue = TreeFormatFlatRuleMultiValue(elems.toList)
}


// --- json capability ---

object TreeFormatRuleObject {

  implicit def ruleValueJson: RootJsonFormat[TreeFormatFlatRuleValue] =
    new RootJsonFormat[TreeFormatFlatRuleValue] {

      override def read(json: JsValue): TreeFormatFlatRuleValue =
        json match {

          case JsArray(elements) =>
            val elemsAsString = elements.map(x => x.convertTo[String]).toList
            TreeFormatFlatRuleMultiValue(elemsAsString)

          case JsString(value) => TreeFormatFlatRuleSingleValue(value)

          case _ => deserializationError("not valid flat rule value json")
        }


      override def write(obj: TreeFormatFlatRuleValue): JsValue =
        obj match {

          case TreeFormatFlatRuleSingleValue(value) => JsString(value)

          case TreeFormatFlatRuleMultiValue(values) => JsArray(values.map(JsString(_)).toVector)

        }

      def toJson[T](obj: T)(implicit w: JsonWriter[T]): JsObject = {
        obj.toJson.asJsObject
      }
    }


  implicit val flatRuleJson: RootJsonFormat[TreeFormatFlatRule] = jsonFormat4(TreeFormatFlatRule)

  implicit val nestedRuleJson: RootJsonFormat[TreeFormatNestedRule] = jsonFormat5(TreeFormatNestedRule)


  implicit def ruleJson: RootJsonFormat[TreeFormatRule] =
    new RootJsonFormat[TreeFormatRule] {

      override def read(json: JsValue): TreeFormatRule =
        Try {
          json.asJsObject.fields.get("field") match {

            case Some(JsString(_)) => json.convertTo[TreeFormatFlatRule]

            case Some(_) => deserializationError("not valid rule json", fieldNames = "field" :: Nil)

            case None => json.convertTo[TreeFormatNestedRule]

          }
        } match {
          case Failure(e) => deserializationError("not valid rule json", e, "field" :: Nil)
          case Success(value) => value
        }

      override def write(obj: TreeFormatRule): JsValue =
        obj match {
          case e: TreeFormatFlatRule => toJson(e)
          case e: TreeFormatNestedRule => toJson(e)
        }

      def toJson[T](obj: T)(implicit w: JsonWriter[T]): JsObject = {
        obj.toJson.asJsObject
      }
    }

}
