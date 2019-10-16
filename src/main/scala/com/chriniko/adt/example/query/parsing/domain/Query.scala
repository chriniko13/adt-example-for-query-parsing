package com.chriniko.adt.example.query.parsing.domain

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZoneOffset}

import com.chriniko.adt.example.query.parsing.Logging
import com.chriniko.adt.example.query.parsing.dto._

import scala.collection.mutable
import spray.json._
import com.chriniko.adt.example.query.parsing.dto.TreeFormatRuleObject._

object Query extends Logging {

  private val stringFormatParsing = (x: String) => x.replace("\"", "'")

  private val treeFormatParsing = (x: String) => {
    val jsonAst = x.parseJson
    val treeFormatRule = jsonAst.convertTo[TreeFormatRule]
    calculateWhere(treeFormatRule)
  }

  private val whereClauseParsingStrategies = Map(
    "string-format" -> stringFormatParsing,
    "tree-format" -> treeFormatParsing
  )

  def generate(sessionId: String,
               queryType: QueryType,
               where: String,
               lastDay: String, daysRange: Int,
               parsingStrategy: String = "tree-format"): SubmittedQuery = {
    val id = Id()
    val dateRange: String = calculateDateRange(lastDay, daysRange)

    val whereClause = whereClauseParsingStrategies(parsingStrategy)(where)

    val query = queryType match {
      case CountQueryType =>
        s"SELECT approx_distinct(maid) " +
          s"FROM audiences " +
          s"WHERE $whereClause AND $dateRange"

      case MaidsQueryType =>
        s"INSERT INTO results " +
          s"SELECT distinct(maid) as maid, '${id.value}' as query_id " +
          s"FROM audiences " +
          s"WHERE $whereClause AND $dateRange"

      case _ => ""

    }
    SubmittedQuery(id, sessionId, queryType, query)
  }

  private val calculateDateRange: (String, Int) => String = (lastDay, daysRange) => {
    val toDay = LocalDate.parse(lastDay)
    val fromDay = toDay.minusDays(daysRange)
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd").withZone(ZoneOffset.UTC)
    val dateRange = s"cast(concat(cast(year as varchar), '-', cast(month as varchar), '-', cast(day as varchar)) as date) " +
      s"BETWEEN date '${formatter.format(fromDay)}' AND date '$lastDay'"
    dateRange
  }

  // ---

  def calculateWhere(rule: TreeFormatRule): String = {

    case class TraversedRule(queryRule: TreeFormatRule, indexInsideFamily: Int, totalFamilyMembers: Int) {
      def isLastInFamily: Boolean = indexInsideFamily == totalFamilyMembers - 1
    }

    def calculateWhereHelper(rootId: String,
                             traversedRule: TraversedRule,
                             acc: mutable.ArrayBuffer[String],
                             parentCombinator: String): Unit = {

      traversedRule match {

        case TraversedRule(TreeFormatFlatRule(id, field, operator, value), _, _) =>
          val valueType = QueryAttribute.getType(field)
          val clause: String = Operator(operator, Value(valueType, value)).perform
          val criteria = s"$field $clause"

          val line = if (id == rootId || traversedRule.isLastInFamily) s" $criteria " else s" $criteria $parentCombinator"
          acc.append(line)


        case TraversedRule(TreeFormatNestedRule(id, _, combinator, rules, _), _, _) =>
          acc.append(" (")

          rules.indices.foreach(idx => {
            val tq = TraversedRule(rules(idx), idx, rules.size)
            calculateWhereHelper(rootId, tq, acc, combinator.toUpperCase())
          })

          if (id == rootId || traversedRule.isLastInFamily) {
            acc.append(") ")
          } else {
            acc.append(s") $parentCombinator")
          }
      }
    }

    val acc = mutable.ArrayBuffer[String]()

    rule match {
      case TreeFormatNestedRule(id, _, combinator, rules, _) =>
        val rootRule = TraversedRule(rule, 0, rules.size)
        calculateWhereHelper(id, rootRule, acc, combinator.toUpperCase)

      case TreeFormatFlatRule(id, _, _, _) =>
        val rootRule = TraversedRule(rule, 0, 0)
        calculateWhereHelper(id, rootRule, acc, "not-needed")
    }

    val queryAsString = acc.mkString
    log.debug(s" >>> calculate query builde" +
      s"r rules format to string result: [ $queryAsString ]")
    queryAsString
  }

  // ----

  sealed trait Value {
    def valueType: Class[_]
  }

  final case class SingleValue(valueType: Class[_], v: String) extends Value

  final case class MultiValue(valueType: Class[_], vs: List[String]) extends Value

  object Value {
    def apply(valueType: Class[_], v: TreeFormatFlatRuleValue): Value = {
      v match {
        case TreeFormatFlatRuleSingleValue(value) => SingleValue(valueType, value)
        case TreeFormatFlatRuleMultiValue(values) => MultiValue(valueType, values)
      }
    }
  }

  // ----

  case object Placeholder {
    val v = "V"
  }

  // ----

  sealed trait Operator {

    val symbol: String
    val value: Value
    val addSingleQuotes: Boolean = true

    def perform: String = {
      value match {
        case SingleValue(t, s) =>
          val replacement = if (t.equals(classOf[String]) && addSingleQuotes) s"'$s'" else s"$s"
          symbol.replace(Placeholder.v, replacement)

        case MultiValue(t, vs) =>
          val f: String => String = x => if (t.equals(classOf[String]) && addSingleQuotes) s"'$x'" else s"$x"
          symbol.replace(Placeholder.v, vs.map(f).mkString(", "))
      }
    }

  }

  object Operator {

    def apply(symbol: String, v: Value): Operator = {
      symbol match {
        case "=" => Equals(v)
        case "!=" => NotEquals(v)

        case "in" => In(v)
        case "notIn" => NotIn(v)

        case "contains" => Contains(v)
        case "notContains" => NotContains(v)
      }
    }
  }

  final case class Equals(value: Value) extends Operator {
    override val symbol: String = s"= ${Placeholder.v}"
  }

  final case class NotEquals(value: Value) extends Operator {
    override val symbol: String = s"!= ${Placeholder.v}"
  }

  final case class In(value: Value) extends Operator {
    override val symbol: String = s"IN (${Placeholder.v})"
  }

  final case class NotIn(value: Value) extends Operator {
    override val symbol: String = s"NOT IN (${Placeholder.v})"
  }

  final case class Contains(value: Value) extends Operator {
    override val addSingleQuotes: Boolean = false

    override val symbol: String = s"LIKE '%${Placeholder.v}%'"
  }

  final case class NotContains(value: Value) extends Operator {
    override val addSingleQuotes: Boolean = false

    override val symbol: String = s"NOT LIKE '%${Placeholder.v}%'"
  }
}
