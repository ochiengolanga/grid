package lib.elasticsearch.impls.elasticsearch1

import lib.elasticsearch.ConditionFixtures
import lib.querysyntax.Negation
import org.elasticsearch.index.query.QueryBuilders._
import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.Json

class QueryBuiderTest extends FunSpec with Matchers with ConditionFixtures {

  val queryBuilder = new QueryBuilder(matchFields = Seq.empty)

  describe("Query builder") {
    it("Nil conditions parameter should give the match all query") {
      val query = queryBuilder.makeQuery(Nil)

      query.toString shouldBe matchAllQuery.toString
    }

    it("empty conditions list should give match all query") {
      val query = queryBuilder.makeQuery(List.empty)

      query.toString shouldBe matchAllQuery.toString
    }

    it("single condition should give a must query") {
      val conditions = List(fieldPhraseMatchCondition)

      val query = queryBuilder.makeQuery(conditions)

      val asJson = Json.parse(query.toString)
      (asJson \ "bool" \\ "must").size shouldBe 1
      (asJson \ "bool" \ "must" \\ "match").size shouldBe 1
      (asJson \ "bool" \ "must" \ "match" \ "afield").isDefined shouldBe true
      (asJson \ "bool" \ "must" \ "match" \ "afield" \ "query").isDefined shouldBe true
      (asJson \ "bool" \ "must" \ "match" \ "afield" \ "query").get.as[String] shouldBe "avalue"
      (asJson \ "bool" \ "must" \ "match" \ "afield" \ "type").get.as[String] shouldBe "phrase"
    }

    it("multiple conditions should give multiple must conditions") {
      val query = queryBuilder.makeQuery(List(fieldPhraseMatchCondition, anotherFieldPhraseMatchCondition))

      val asJson = Json.parse(query.toString)
      (asJson \ "bool" \\ "must").size shouldBe 1
      (asJson \ "bool" \ "must" \\ "match").size shouldBe 2
    }

    it("negated conditions should be expressed using must not clauses") {
      val negatedCondition = Negation(fieldPhraseMatchCondition)

      val query = queryBuilder.makeQuery(List(negatedCondition))

      val asJson = Json.parse(query.toString)
      (asJson \ "bool" \\ "must_not").size shouldBe 1
      (asJson \ "bool" \ "must_not" \ "match" \ "afield" \ "query").get.as[String] shouldBe "avalue"
      (asJson \ "bool" \ "must_not" \ "match" \ "afield" \ "type").get.as[String] shouldBe "phrase"
    }
  }

}
