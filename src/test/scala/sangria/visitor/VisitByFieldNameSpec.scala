package sangria.visitor

import sangria.visitor.util.StringMatchers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class VisitAnyFieldByNameSpec extends AnyWordSpec with Matchers with StringMatchers {
  import VisitAnyFieldByNameSpec._

  "VisitAnyFieldByName command" should {
    "visit special fields based on parent type and member names" in {
      val field = Field(
        "foo",
        Value(NonAstField(0), NonAstField(0)),
        NonAstField(0)
      )
      val res = visit[Ast](
        field,
        VisitAnyFieldByName[Value, NonAstField]("special", (_, f) => VisitorCommand.Transform(NonAstField(f.value + 1)))
      )
      res should be(Field("foo", Value(NonAstField(0), NonAstField(1)), NonAstField(0)))
    }
  }
}

object VisitAnyFieldByNameSpec {

  case class NonAstField(value: Int)

  sealed trait Ast
  case class Field(name: String, value: Value, special: NonAstField) extends Ast
  case class Value(field: NonAstField, special: NonAstField) extends Ast
}