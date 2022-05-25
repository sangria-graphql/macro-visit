package sangria.visitor

import sangria.visitor.util.StringMatchers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NonCollectionVisitorSpec extends AnyWordSpec with Matchers with StringMatchers {
  import NonCollectionVisitorSpec._

  // TODO onLeave
  "Simple Visitor without collections" should {
    val f = Field(
      "start",
      StringValue("example"),
      IntValue(1)
    )
    
    "transform values" in {
      val res = visit[Ast](
        f,
        Visit[StringValue](f => VisitorCommand.Transform(f.copy(value = "changed")))
      )
      res should be(Field("start", StringValue("changed"), IntValue(1)))
    }

    "not delete values" in {
      val res = visit[Ast](
        f,
        Visit[StringValue](f => VisitorCommand.Delete)
      )
      res should be(Field("start", StringValue("example"), IntValue(1)))
    }

    "ignore nulls" in {
      val withNulls = Field(null, StringValue(null), null)
      val res = visit[Ast](
        withNulls,
        Visit[StringValue](f => VisitorCommand.Transform(f.copy(value = "changed")))
      )
      res should be(Field(null, StringValue("changed"), null))
    }

    "handle Skip" in {
      val field =
        Field(
          "foo",
          FieldValue(Field("skip", IntValue(128), StringValue("example"))),
          FieldValue(Field("no skip", IntValue(128), StringValue("example")))
        )
      val res = visit[Ast](
        field,
        Visit[IntValue](f => VisitorCommand.Transform(IntValue(f.value + 1))),
        Visit[StringValue](f => VisitorCommand.Transform(StringValue("changed"))),
        Visit[Field](f => if (f.name == "skip") VisitorCommand.Skip else VisitorCommand.Continue)
      )
      res should be(
        Field(
          "foo",
          FieldValue(Field("skip", IntValue(128), StringValue("example"))),
          FieldValue(Field("no skip", IntValue(129), StringValue("changed"))),
        )
      )
    }

    
  }
}

object NonCollectionVisitorSpec {

  sealed trait Ast
  case class Field(name: String, value1: Value, value2: Value) extends Ast
  sealed trait Value extends Ast
  case class StringValue(value: String) extends Value
  case class IntValue(value: Int) extends Value
  case class FieldValue(value: Field) extends Value

}