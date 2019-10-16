package com.chriniko.adt.example.query.parsing.domain

import com.chriniko.adt.example.query.parsing.dto.{TreeFormatFlatRule, TreeFormatFlatRuleMultiValue, TreeFormatFlatRuleSingleValue, TreeFormatNestedRule}
import org.scalatest.FunSuite

import scala.util.{Failure, Success, Try}

class QueryTest extends FunSuite {

  test("generate query for type count, method works as expected normal case") {

    // given
    val whereClause = "(color == \"yellow\" AND color2 NOT IN (\"purple\",\"green\") AND stock == true AND (t1.subname == \"yolo\" OR name2 == \"trol\"))"

    // when
    val submittedQuery = Query.generate("sessionId", CountQueryType, whereClause, "2019-10-07", 2, "string-format")

    // then
    assert(
      submittedQuery.query ==
        "SELECT approx_distinct(maid) " +
          "FROM audiences " +
          "WHERE (color == 'yellow' AND color2 NOT IN ('purple','green') AND stock == true AND (t1.subname == 'yolo' OR name2 == 'trol')) " +
          "AND cast(concat(cast(year as varchar), '-', cast(month as varchar), '-', cast(day as varchar)) as date) BETWEEN date '2019-10-05' AND date '2019-10-07'")

  }

  test("generate query for type maids, method works as expected normal case") {

    // given
    val whereClause = "(color == \"yellow\" AND color2 NOT IN (\"purple\",\"green\") AND stock == true AND (t1.subname == \"yolo\" OR name2 == \"trol\"))"

    // when
    val submittedQuery = Query.generate("sessionId", MaidsQueryType, whereClause, "2019-10-07", 2, "string-format")

    val idValue = submittedQuery.id.value

    // then
    assert(
      submittedQuery.query ==
        "INSERT INTO results SELECT distinct(maid) as maid, '" + idValue + "' as query_id " +
          "FROM audiences " +
          "WHERE (color == 'yellow' AND color2 NOT IN ('purple','green') AND stock == true AND (t1.subname == 'yolo' OR name2 == 'trol')) " +
          "AND cast(concat(cast(year as varchar), '-', cast(month as varchar), '-', cast(day as varchar)) as date) BETWEEN date '2019-10-05' AND date '2019-10-07'")

  }

  test("generate query for type unknown, method works as expected normal case") {

    // given
    val whereClause = "(color == \"yellow\" AND color2 NOT IN (\"purple\",\"green\") AND stock == true AND (t1.subname == \"yolo\" OR name2 == \"trol\"))"


    Try {
      // when
      Query.generate("sessionId", UnknownQueryType, whereClause, "2019-10-07", 2)
    } match {
      // then
      case Failure(exception) => assert(exception != null)
      case Success(_) => fail()
    }

  }

  test("calculate(TreeFormatRule) method works as expected normal case 1") {

    // given
    val dto = TreeFormatNestedRule(
      "k-1adca804-ca44-4eed-8c70-f86f641c7263", None, "or", List(

        TreeFormatFlatRule("a-06b0b76b-6e09-4a2a-aafd-8b7dfa6805ce", "country", "=", TreeFormatFlatRuleSingleValue("ALB")),
        TreeFormatFlatRule("b-1322beca-4fe8-4d79-8a99-e0de1f3fd318", "os", "=", TreeFormatFlatRuleSingleValue("Android")),
        TreeFormatFlatRule("c-624e9756-245f-4650-a46c-1a50867edb35", "yob", "=", TreeFormatFlatRuleSingleValue("1992")),

        TreeFormatNestedRule(
          "d-57b18e6e-b2c7-47d5-87ac-ab4b97d4e635", None, "or", List(

            TreeFormatFlatRule("e-af418c92-7d67-4966-8e9f-cd5602f948bd", "maid", "=", TreeFormatFlatRuleSingleValue("123")),
            TreeFormatFlatRule("f-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "category", "=", TreeFormatFlatRuleSingleValue("IAB1-1")),
            TreeFormatFlatRule("g-3584620d-982c-4942-81e3-600b68047069", "language", "=", TreeFormatFlatRuleSingleValue("AY")),

            TreeFormatNestedRule("h-dbd5ee61-dd8c-4ef3-9112-9762073b90ef", None, "and", List(
              TreeFormatFlatRule("i-025ad900-7e25-4d05-ba46-c8a7e8c8ea56", "bundle", "contains", TreeFormatFlatRuleSingleValue("dmp")),
              TreeFormatFlatRule("j-025ad900-7e25-4d05-ba46-c8a7e8c8ea56", "osv", "contains", TreeFormatFlatRuleSingleValue("11.2"))), false
            )
          ), false
        )
      ), false
    )

    // when
    val result = Query.calculateWhere(dto)

    // then
    assert(result == " ( country = 'ALB' OR os = 'Android' OR yob = 1992 OR ( maid = '123' OR category = 'IAB1-1' OR language = 'AY' OR ( bundle LIKE '%dmp%' AND osv LIKE '%11.2%' ) ) ) ")

  }

  test("calculate(TreeFormatRule) method works as expected normal case 2") {

    // given
    val dto = TreeFormatNestedRule(
      "g-1adca804-ca44-4eed-8c70-f86f641c7263", None, "or", List(
        TreeFormatFlatRule("r-06b0b76b-6e09-4a2a-aafd-8b7dfa6805ce", "country", "=", TreeFormatFlatRuleSingleValue("ALB"))
      ), false
    )

    // when
    val result = Query.calculateWhere(dto)

    // then
    assert(result == " ( country = 'ALB' ) ")

  }

  test("calculate(TreeFormatRule) method works as expected normal case 3") {

    // given
    val dto = TreeFormatNestedRule(
      "g-1adca804-ca44-4eed-8c70-f86f641c7263", None, "or", List(
        TreeFormatFlatRule("r-06b0b76b-6e09-4a2a-aafd-8b7dfa6805ce", "country", "=", TreeFormatFlatRuleSingleValue("ALB")),
        TreeFormatFlatRule("r-1322beca-4fe8-4d79-8a99-e0de1f3fd318", "os", "=", TreeFormatFlatRuleSingleValue("Android")),
      ), false
    )

    // when
    val result = Query.calculateWhere(dto)

    // then
    assert(result == " ( country = 'ALB' OR os = 'Android' ) ")

  }

  test("calculate(TreeFormatRule) method works as expected normal case 4") {

    // given
    val dto = TreeFormatNestedRule(
      "g-1adca804-ca44-4eed-8c70-f86f641c7263", None, "or", List(

        TreeFormatNestedRule(
          "g-1adca804-ca43-4eed-8c70-f86f641c7263", None, "and", List(

            TreeFormatNestedRule(
              "g-1adca804-ca43-4eed-8c70-f86f641c7263", None, "and", List(

                TreeFormatFlatRule("r-06b0b76b-6e09-4a2a-aafd-8b7dfa6805ce", "country", "=", TreeFormatFlatRuleSingleValue("ALB"))

              ), false
            )
          ), false
        )
      ), false
    )

    // when
    val result = Query.calculateWhere(dto)

    // then
    assert(result == " ( ( ( country = 'ALB' ) ) ) ")

  }

  test("calculate(TreeFormatRule) method works as expected normal case 5") {

    // given
    val dto = TreeFormatNestedRule(

      "g-1adca804-ca44-4eed-8c70-f86f641c7263", None, "or", List(

        TreeFormatNestedRule(
          "g-1adca804-ca43-4eed-8c70-f86f641c7263", None, "and", List(
            TreeFormatFlatRule("r-06b0b76b-6e09-4a2a-aafd-8b7dfa6805ce", "country", "=", TreeFormatFlatRuleSingleValue("ALB"))
          ), false
        ),

        TreeFormatFlatRule("r-1322beca-4fe8-4d79-8a99-e0de1f3fd318", "os", "=", TreeFormatFlatRuleSingleValue("Android")),
      ), false
    )

    // when
    val result = Query.calculateWhere(dto)

    // then
    assert(result == " ( ( country = 'ALB' ) OR os = 'Android' ) ")

  }

  test("calculate(TreeFormatRule) method works as expected normal case 6") {

    // given
    val dto = TreeFormatNestedRule(

      "g-1adca804-ca44-4eed-8c70-f86f641c7263", None, "or", List(

        TreeFormatNestedRule(
          "x-1adca804-ca44-4eed-8c70-f86f641c7163", None, "and", List(
            TreeFormatFlatRule("a-16b0b76b-6e09-4a2a-aafd-8b7dfa6805ce", "country", "=", TreeFormatFlatRuleSingleValue("ALB"))
          ), false
        ),

        TreeFormatNestedRule(
          "h-1adca804-ca44-4eed-8c70-f86f841c7263", None, "or", List(
            TreeFormatFlatRule("y-1110b76b-6e09-4a1a-aafd-8b7dfa6805ce", "gender", "=", TreeFormatFlatRuleSingleValue("female"))
          ), false
        )
      ), false
    )

    // when
    val result = Query.calculateWhere(dto)

    // then
    assert(result == " ( ( country = 'ALB' ) OR ( gender = 'female' ) ) ")

  }

  test("calculate(TreeFormatRule) method works as expected normal case 7") {

    // given
    val dto = TreeFormatNestedRule(
      "g-1adca804-ca44-4eed-8c70-f86f641c7263", None, "and", List(

        TreeFormatFlatRule("r-1322beca-4fe8-4d79-8a99-e0de1f3fd318", "os", "=", TreeFormatFlatRuleSingleValue("Android")),
        TreeFormatFlatRule("r-624e9756-245f-4650-a46c-1a50867edb35", "yob", "=", TreeFormatFlatRuleSingleValue("1992")),

        TreeFormatNestedRule(
          "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e635", None /*frequency appears again, in already defined nested rule frequency*/ , "or", List(

            TreeFormatFlatRule("r-af418c92-7d67-4966-8e9f-cd5602f948bd", "maid", "=", TreeFormatFlatRuleSingleValue("123")),
            TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "category", "=", TreeFormatFlatRuleSingleValue("IAB1-1")),
          ), false
        )
      ), false
    )

    // when
    val result = Query.calculateWhere(dto)

    // then
    assert(result == " ( os = 'Android' AND yob = 1992 AND ( maid = '123' OR category = 'IAB1-1' ) ) ")

  }

  test("calculate(TreeFormatRule) method works as expected normal case 8") {

    // given
    val dto = TreeFormatNestedRule(
      "g-1adca804-ca44-4eed-8c70-f86f641c7263", None, "and", List(

        TreeFormatFlatRule("r-1322beca-4fe8-4d79-8a99-e0de1f3fd318", "os", "=", TreeFormatFlatRuleSingleValue("Android")),
        TreeFormatFlatRule("r-624e9756-245f-4650-a46c-1a50867edb35", "yob", "=", TreeFormatFlatRuleSingleValue("1992")),

        TreeFormatNestedRule(
          "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e635", None, "or", List(

            TreeFormatFlatRule("r-af418c92-7d67-4966-8e9f-cd5602f948bd", "maid", "=", TreeFormatFlatRuleSingleValue("123")),
            TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "category", "=", TreeFormatFlatRuleSingleValue("IAB1-1")),
          ), false
        )
      ), false
    )

    // when
    val result = Query.calculateWhere(dto)

    // then
    assert(result == " ( os = 'Android' AND yob = 1992 AND ( maid = '123' OR category = 'IAB1-1' ) ) ")

  }

  test("calculate(TreeFormatRule) method works as expected normal case 9") {

    // given
    val dto = TreeFormatNestedRule(
      "g-1adca804-ca44-4eed-8c70-f86f641c7263", None, "and", List(

        TreeFormatFlatRule("r-1322beca-4fe8-4d79-8a99-e0de1f3fd318", "os", "=", TreeFormatFlatRuleSingleValue("Android")),
        TreeFormatFlatRule("r-624e9756-245f-4650-a46c-1a50867edb35", "yob", "=", TreeFormatFlatRuleSingleValue("1992")),

        TreeFormatNestedRule(
          "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e635", None, "or", List(

            TreeFormatFlatRule("r-af418c92-7d67-4966-8e9f-cd5602f948bd", "maid", "=", TreeFormatFlatRuleSingleValue("123")),
            TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "category", "=", TreeFormatFlatRuleSingleValue("IAB1-1")),
          ), false
        ),

        TreeFormatNestedRule(
          "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e635", None, "or", List(

            TreeFormatFlatRule("r-af418c92-7d67-4966-8e9f-cd5602f948bd", "maid", "=", TreeFormatFlatRuleSingleValue("1234")),
            TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "category", "=", TreeFormatFlatRuleSingleValue("IAB1-12")),
          ), false
        )
      ), false
    )

    // when
    val result = Query.calculateWhere(dto)

    // then
    assert(result == " ( os = 'Android' AND yob = 1992 AND ( maid = '123' OR category = 'IAB1-1' ) AND ( maid = '1234' OR category = 'IAB1-12' ) ) ")

  }

  test("calculate(TreeFormatRule) method works as expected normal case 10") {

    // given
    val dto = TreeFormatNestedRule(
      "g-1adca804-ca44-4eed-8c70-f86f641c7260", None, "and", List(

        TreeFormatFlatRule("r-1322beca-4fe8-4d79-8a99-e0de1f3fd311", "os", "=", TreeFormatFlatRuleSingleValue("Android")),
        TreeFormatFlatRule("r-624e9756-245f-4650-a46c-1a50867edb32", "yob", "=", TreeFormatFlatRuleSingleValue("1992")),

        TreeFormatNestedRule(
          "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e633", None, "or", List(

            TreeFormatFlatRule("r-af418c92-7d67-4966-8e9f-cd5602f948b4", "maid", "=", TreeFormatFlatRuleSingleValue("123")),
            TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "category", "=", TreeFormatFlatRuleSingleValue("IAB1-1")),
          ), false
        ),

        TreeFormatNestedRule(
          "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e636", None, "or", List(

            TreeFormatFlatRule("r-af418c92-7d67-4966-8e9f-cd5602f948b7", "maid", "=", TreeFormatFlatRuleSingleValue("1234")),
            TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba1e1ec48", "category", "=", TreeFormatFlatRuleSingleValue("IAB1-12")),

            TreeFormatNestedRule(
              "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e637", None, "and", List(
                TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba1e1ec49", "gender", "=", TreeFormatFlatRuleSingleValue("female")),
                TreeFormatFlatRule("b-a2bc8cef-967b-45f9-8186-2fdba1e1ec10", "osv", "=", TreeFormatFlatRuleSingleValue("1.24")),
                TreeFormatFlatRule("c-a2bc8cef-967b-45f9-8186-2fdba1e1ec11", "country", "=", TreeFormatFlatRuleSingleValue("GR")),
              ), false
            )
          ), false
        )

      ), false
    )

    // when
    val result = Query.calculateWhere(dto)

    // then
    assert(result == " ( os = 'Android' AND yob = 1992 AND ( maid = '123' OR category = 'IAB1-1' ) AND ( maid = '1234' OR category = 'IAB1-12' OR ( gender = 'female' AND osv = '1.24' AND country = 'GR' ) ) ) ")

  }

  test("calculate(TreeFormatRule) method works as expected normal case 11") {

    // given
    val dto = TreeFormatNestedRule(
      "g-1adca804-ca44-4eed-8c70-f86f641c7263", None, "and", List(

        TreeFormatNestedRule(
          "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e635", None, "or", List(

            TreeFormatFlatRule("r-af418c92-7d67-4966-8e9f-cd5602f948bd", "maid", "=", TreeFormatFlatRuleSingleValue("123")),
            TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "category", "=", TreeFormatFlatRuleSingleValue("IAB1-1")),
          ), false
        ),

        TreeFormatNestedRule(
          "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e635", None, "or", List(

            TreeFormatFlatRule("r-af418c92-7d67-4966-8e9f-cd5602f948bd", "maid", "=", TreeFormatFlatRuleSingleValue("1234")),
            TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "category", "=", TreeFormatFlatRuleSingleValue("IAB1-12")),

            TreeFormatNestedRule(
              "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e637", None, "and", List(
                TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "gender", "=", TreeFormatFlatRuleSingleValue("female")),
                TreeFormatFlatRule("b-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "osv", "=", TreeFormatFlatRuleSingleValue("1.24")),
                TreeFormatFlatRule("c-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "country", "=", TreeFormatFlatRuleSingleValue("GR")),
              ), false
            )
          ), false
        ),


        TreeFormatFlatRule("r-1322beca-4fe8-4d79-8a99-e0de1f3fd318", "os", "=", TreeFormatFlatRuleSingleValue("Android")),

        TreeFormatFlatRule("r-624e9756-245f-4650-a46c-1a50867edb35", "yob", "=", TreeFormatFlatRuleSingleValue("1992"))

      ), false
    )

    // when
    val result = Query.calculateWhere(dto)

    // then
    assert(result == " ( ( maid = '123' OR category = 'IAB1-1' ) AND ( maid = '1234' OR category = 'IAB1-12' OR ( gender = 'female' AND osv = '1.24' AND country = 'GR' ) ) AND os = 'Android' AND yob = 1992 ) ")

  }

  test("calculate(TreeFormatRule) method works as expected normal case 12") {

    // given
    val dto = TreeFormatNestedRule(
      "g-1adca804-ca44-4eed-8c70-f86f641c7263", None, "and", List(), false
    )

    // when
    val result = Query.calculateWhere(dto)

    // then
    assert(result == " () ")

  }

  test("calculate(TreeFormatRule) method works as expected normal case 13") {

    // given
    val dto = TreeFormatNestedRule(
      "g-1adca804-ca44-4eed-8c70-f86f641c7263", None, "and", List(

        TreeFormatNestedRule(
          "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e635", None, "or", List(

            TreeFormatFlatRule("r-af418c92-7d67-4966-8e9f-cd5602f948bd", "maid", "=", TreeFormatFlatRuleSingleValue("123")),
            TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "category", "=", TreeFormatFlatRuleSingleValue("IAB1-1")),
          ), false
        ),

        TreeFormatNestedRule(
          "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e635", None, "or", List(

            TreeFormatFlatRule("r-af418c92-7d67-4966-8e9f-cd5602f948bd", "maid", "=", TreeFormatFlatRuleSingleValue("1234")),
            TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "category", "=", TreeFormatFlatRuleSingleValue("IAB1-12")),

            TreeFormatNestedRule(
              "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e637", None, "and", List(
                TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "gender", "=", TreeFormatFlatRuleSingleValue("female")),
                TreeFormatFlatRule("b-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "osv", "=", TreeFormatFlatRuleSingleValue("1.24")),

                TreeFormatNestedRule(
                  "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e637", None, "or", List(
                    TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba121ec45", "attr1", "=", TreeFormatFlatRuleSingleValue("val1")),
                    TreeFormatFlatRule("b-a2bc8cef-967b-45f9-8186-2fdba131ec45", "attr2", "=", TreeFormatFlatRuleSingleValue("val2")),
                    TreeFormatFlatRule("c-a2bc8cef-967b-45f9-8186-2fdba141ec45", "attr3", "=", TreeFormatFlatRuleSingleValue("val3")),
                  ), false
                )
              ), false
            )
          ), false
        ),


        TreeFormatFlatRule("r-1322beca-4fe8-4d79-8a99-e0de1f3fd318", "os", "=", TreeFormatFlatRuleSingleValue("Android")),

        TreeFormatNestedRule(
          "g-57b18e6e-abcd-47d5-87ac-ab4b97d4e637", None, "or", List(
            TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba121ec45", "attr1", "=", TreeFormatFlatRuleSingleValue("val1")),

            TreeFormatNestedRule(
              "g-57b18e6e-efg-47d5-87ac-ab4b97d4e637", None, "and", List(
                TreeFormatFlatRule("r-a2bc8cef-efg1-45f9-8186-2fdba121ec45", "attr2.1", "=", TreeFormatFlatRuleSingleValue("val1")),
                TreeFormatFlatRule("b-a2bc8cef-efg2-45f9-8186-2fdba131ec45", "attr2.2", "=", TreeFormatFlatRuleSingleValue("val2")),
                TreeFormatFlatRule("c-a2bc8cef-efg3-45f9-8186-2fdba141ec45", "attr2.3", "=", TreeFormatFlatRuleSingleValue("val3")),
              ), false
            ),

            TreeFormatFlatRule("c-a2bc8cef-efg456-45f9-8186-2fdba141ec45", "attr3", "=", TreeFormatFlatRuleSingleValue("val3")),
          ), false
        )

      ), false
    )

    // when
    val result = Query.calculateWhere(dto)

    // then
    assert(result == " ( ( maid = '123' OR category = 'IAB1-1' ) AND ( maid = '1234' OR category = 'IAB1-12' OR ( gender = 'female' AND osv = '1.24' AND ( attr1 = 'val1' OR attr2 = 'val2' OR attr3 = 'val3' ) ) ) AND os = 'Android' AND ( attr1 = 'val1' OR ( attr2.1 = 'val1' AND attr2.2 = 'val2' AND attr2.3 = 'val3' ) OR attr3 = 'val3' ) ) ")

  }

  test("calculate(TreeFormatRule) method works as expected normal case 14 with multivalues") {

    // given
    val dto = TreeFormatNestedRule(
      "g-1adca804-ca44-4eed-8c70-f86f641c7263", None, "and", List(

        TreeFormatFlatRule("r-1322beca-4fe8-4d79-8a99-e0de1f3fd318", "os", "in", TreeFormatFlatRuleMultiValue("Android", "IOS")),

        TreeFormatFlatRule("r-624e9756-245f-4650-a46c-1a50867edb35", "yob", "=", TreeFormatFlatRuleSingleValue("1992")),

        TreeFormatNestedRule(
          "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e635", None, "or", List(

            TreeFormatFlatRule("r-af418c92-7d67-4966-8e9f-cd5602f948bd", "maid", "=", TreeFormatFlatRuleSingleValue("123")),
            TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "category", "in", TreeFormatFlatRuleMultiValue("IAB1-1", "IAB1-2", "IAB1-3")),
          ), false
        ),

        TreeFormatNestedRule(
          "g-57b18e6e-b2c7-47d5-87ac-ab4b97d4e635", None, "or", List(

            TreeFormatFlatRule("r-af418c92-7d67-4966-8e9f-cd5602f948bd", "maid", "=", TreeFormatFlatRuleSingleValue("1234")),
            TreeFormatFlatRule("r-a2bc8cef-967b-45f9-8186-2fdba1e1ec45", "category", "=", TreeFormatFlatRuleSingleValue("IAB1-12")),
          ), false
        )
      ), false
    )

    // when
    val result = Query.calculateWhere(dto)

    // then
    assert(result == " ( os IN ('Android', 'IOS') AND yob = 1992 AND ( maid = '123' OR category IN ('IAB1-1', 'IAB1-2', 'IAB1-3') ) AND ( maid = '1234' OR category = 'IAB1-12' ) ) ")

  }

  test("calculate(TreeFormatRule) method works as expected normal case 15") {

    // given
    val dto = TreeFormatFlatRule("r-1322beca-4fe8-4d79-8a99-e0de1f3fd318", "os", "in", TreeFormatFlatRuleMultiValue("Android","IOS"))


    // when
    val result = Query.calculateWhere(dto)

    // then
    assert(result == " os IN ('Android', 'IOS') ")
  }

}
