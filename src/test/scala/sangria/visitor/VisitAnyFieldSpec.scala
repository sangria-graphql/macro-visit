package sangria.visitor

import sangria.visitor.util.StringMatchers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class VisitAnyFieldSpec extends AnyWordSpec with Matchers with StringMatchers {
  import VisitAnyFieldSpec._

  "VisitAnyField command" should {
    "visit special fields based on parent type" in {
      val field = Field("foo", FieldValue(Field("bar", IntValue(32), NonAstField(64), None), NonAstField(128), None), NonAstField(32), None)
      val res = visit[Ast](
        field,
        VisitAnyField[Field, NonAstField]((a, f) => VisitorCommand.Transform(NonAstField(f.value + 1)))
      )
      res should be(
        Field("foo", FieldValue(Field("bar", IntValue(32), NonAstField(65), None), NonAstField(128), None), NonAstField(33), None)
      )
    }

    "handle Option correctly" in {
      val field = Field(
        "foo", 
        FieldValue(
          Field("bar", IntValue(32), NonAstField(64), Some(NonAstField(64))),
          NonAstField(128),
          Some(NonAstField(128))
        ),
        NonAstField(32),
        Some(NonAstField(32))
      )

      val res = visit[Ast](
        field,
        VisitAnyField[Field, Option[NonAstField]]((_, f) => VisitorCommand.Transform(f.map(nonAstField => NonAstField(nonAstField.value + 1))))
      )
      res should be(
        Field("foo", FieldValue(Field("bar", IntValue(32), NonAstField(64), Some(NonAstField(65))), NonAstField(128), Some(NonAstField(128))), NonAstField(32), Some(NonAstField(33)))
      )
    }
  }
}

object VisitAnyFieldSpec {

  case class NonAstField(value: Int)

  sealed trait Ast
  case class Field(name: String, value: Value, nonAst: NonAstField, maybeNonAst: Option[NonAstField]) extends Ast
  sealed trait Value extends Ast
  case class IntValue(value: Int) extends Value
  case class FieldValue(fieldVal: Field, nonAst: NonAstField, maybeNonAst: Option[NonAstField]) extends Value
}