package com.chriniko.adt.example.query.parsing.dto

import com.chriniko.adt.example.query.parsing.dto.TreeFormatRuleObject._
import org.scalatest.FunSuite
import spray.json._

class TreeFormatRuleTest extends FunSuite {


  test("serialization / deserialization works as expected") {

    // given
    val jsonAst = treeFormatRuleAsString.parseJson

    // when
    val treeFormatRule = jsonAst.convertTo[TreeFormatRule]

    // then
    assert(
      treeFormatRule ==

        TreeFormatNestedRule(
          "g-1adca804-ca44-4eed-8c70-f86f641c7263", None, "or", List(

            TreeFormatFlatRule("r-06b0b76b-6e09-4a2a-aafd-8b7dfa6805ce", "country", "=", TreeFormatFlatRuleSingleValue("ALB")),

            TreeFormatFlatRule("r-1322beca-4fe8-4d79-8a99-e0de1f3fd318", "os", "=", TreeFormatFlatRuleSingleValue("")),

            TreeFormatFlatRule("r-624e9756-245f-4650-a46c-1a50867edb35", "yob", "=", TreeFormatFlatRuleSingleValue("1992")),

            TreeFormatNestedRule(
              "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e635", None, "or", List(

                TreeFormatFlatRule("r-af418c92-7d67-4966-8e9f-cd5602f948bd", "maid", "=", TreeFormatFlatRuleSingleValue("123")),
                TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "category", "=", TreeFormatFlatRuleSingleValue("IAB1-1")),
                TreeFormatFlatRule("r-3584620d-982c-4942-81e3-600b68047069", "language", "=", TreeFormatFlatRuleSingleValue("AY")),

                TreeFormatNestedRule("g-dbd5ee61-dd8c-4ef3-9112-9762073b90ef", None, "and", List(
                  TreeFormatFlatRule("r-025ad900-7e25-4d05-ba46-c8a7e8c8ea56", "bundle", "contains", TreeFormatFlatRuleSingleValue("dmp"))), false)), false)
          ), false
        )

    )


    // when
    val serialized = treeFormatRule.toJson.toString()


    // then
    assert(treeFormatRuleAsString.parseJson == serialized.parseJson)

  }

  test("serialization / deserialization works as expected, multi value case") {


    // given
    val payload = "{\"id\":\"g-9UGrGS73FV34z-EPguzWq\",\"rules\":[{\"id\":\"r-Wta29cZGO7Ax3JqNhOYsH\",\"field\":\"country\",\"value\":[\"AFG\",\"ALA\",\"ALB\"],\"operator\":\"in\"}],\"combinator\":\"and\",\"not\":false}"


    // when
    val jsonAst = payload.parseJson
    val treeFormatRule = jsonAst.convertTo[TreeFormatRule]


    // then
    println(treeFormatRule)

    assert(
      treeFormatRule ==

        TreeFormatNestedRule("g-9UGrGS73FV34z-EPguzWq", None, "and",
          List(
            TreeFormatFlatRule("r-Wta29cZGO7Ax3JqNhOYsH", "country", "in", TreeFormatFlatRuleMultiValue(List("AFG", "ALA", "ALB")))
          ),
          not = false)

    )

    // when
    val serialized = treeFormatRule.toJson.toString()


    // then
    val expected = payload.parseJson
    val actual = serialized.parseJson
    assert(expected == actual)

  }


  // ---

  val treeFormatRuleAsString: String =
    """
      {
        "id": "g-1adca804-ca44-4eed-8c70-f86f641c7263",
        "rules": [
          {
            "id": "r-06b0b76b-6e09-4a2a-aafd-8b7dfa6805ce",
            "field": "country",
            "value": "ALB",
            "operator": "="
          },
          {
            "id": "r-1322beca-4fe8-4d79-8a99-e0de1f3fd318",
            "field": "os",
            "value": "",
            "operator": "="
          },
          {
            "id": "r-624e9756-245f-4650-a46c-1a50867edb35",
            "field": "yob",
            "value": "1992",
            "operator": "="
          },
          {
            "id": "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e635",
            "rules": [
              {
                "id": "r-af418c92-7d67-4966-8e9f-cd5602f948bd",
                "field": "maid",
                "value": "123",
                "operator": "="
              },
              {
                "id": "r-a2bc8cef-967b-45f9-8186-2fdba1e1ec45",
                "field": "category",
                "value": "IAB1-1",
                "operator": "="
              },
              {
                "id": "r-3584620d-982c-4942-81e3-600b68047069",
                "field": "language",
                "value": "AY",
                "operator": "="
              },
              {
                "id": "g-dbd5ee61-dd8c-4ef3-9112-9762073b90ef",
                "rules": [
                  {
                    "id": "r-025ad900-7e25-4d05-ba46-c8a7e8c8ea56",
                    "field": "bundle",
                    "value": "dmp",
                    "operator": "contains"
                  }
                ],
                "combinator": "and",
                "not": false
              }
            ],
            "combinator": "or",
            "not": false
          }
        ],
        "combinator": "or",
        "not": false
      }
    """.stripMargin

}
