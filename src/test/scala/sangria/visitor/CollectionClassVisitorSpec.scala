package sangria.visitor

import sangria.visitor.util.StringMatchers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CollectionVisitorSpec extends AnyWordSpec with Matchers with StringMatchers {
  import CollectionVisitorSpec._

  // TODO onLeave
  "Simple Visitor with Option" should {

    "transform objects in Options" in {
      val field = Field("foo", StringMaybeValue(Some("bar")), Some(IntMaybeValue(Some(10))))
      val res = visit[Ast](
        field,
        Visit[StringMaybeValue](f => VisitorCommand.Transform(f.copy(value = None))),
        Visit[IntMaybeValue](f => VisitorCommand.Transform(IntMaybeValue(None)))
      )
      res should be(Field("foo", StringMaybeValue(None), Some(IntMaybeValue(None))))
    }

    "handle recursion" in {
      val rField = RField("foo", Some(RField("bar", None, RIntValue(1))), RStringValue("Foo"))
      val res = visit[RAst](
        rField,
        Visit[RField](f =>
          if (f.name == "foo") VisitorCommand.Transform(f.copy(value = RIntValue(2)))
          else VisitorCommand.Continue),
        Visit[RField](f =>
          if (f.name == "bar") VisitorCommand.Transform(f.copy(value = RStringValue("Bar")))
          else VisitorCommand.Continue)
      )
      res should be(RField("foo", Some(RField("bar", None, RStringValue("Bar"))), RIntValue(2)))
    }

    "delete Option contents" in {
      val field = RField("foo", Some(RField("delete", None, RIntValue(128))), RStringValue("bar"))
      val res = visit[RAst](
        field,
        Visit[RField](v =>
          if (v.name == "delete") VisitorCommand.Delete else VisitorCommand.Continue)
      )
      res should be(RField("foo", None, RStringValue("bar")))
    }
  }

  "Simple Visitor with List" should {

    "transform non Ast values correctly" in {
      val field = ListField("foo", Nil, List(ListIntValue(64), ListIntValue(32)))
      val res = visit[ListAst](
        field,
        Visit[ListIntValue](v => VisitorCommand.Transform(ListIntValue(v.value + 1)))
      )
      res should be(ListField("foo", Nil, List(ListIntValue(65), ListIntValue(33))))
    }

    "transform recursively correctly" in {
      val field = ListField(
        "foo",
        List(ListField("bar", Nil, List(ListIntValue(32), ListIntValue(128)))),
        List(ListIntValue(64)))
      val res = visit[ListAst](
        field,
        Visit[ListIntValue](v => VisitorCommand.Transform(ListIntValue(v.value + 1)))
      )
      res should be(
        ListField(
          "foo",
          List(ListField("bar", Nil, List(ListIntValue(33), ListIntValue(129)))),
          List(ListIntValue(65))))
    }

    "delete List contents" in {
      val field = ListField(
        "foo",
        List(
          ListField("delete", Nil, Nil),
          ListField("bar", Nil, List(ListIntValue(128), ListIntValue(129)))),
        List(ListIntValue(128)))
      val res = visit[ListAst](
        field,
        Visit[ListIntValue](v =>
          if (v.value == 128) VisitorCommand.Delete else VisitorCommand.Continue),
        Visit[ListField](v =>
          if (v.name == "delete") VisitorCommand.Delete else VisitorCommand.Continue)
      )
      res should be(ListField("foo", List(ListField("bar", Nil, List(ListIntValue(129)))), Nil))
    }
  }
}

object CollectionVisitorSpec {
  sealed trait Ast
  case class Field(name: String, value1: Value, value2: Option[Value]) extends Ast
  sealed trait Value extends Ast
  case class StringMaybeValue(value: Option[String]) extends Value
  case class IntMaybeValue(value: Option[Int]) extends Value

  sealed trait RAst
  case class RField(name: String, next: Option[RField], value: RValue) extends RAst
  sealed trait RValue extends RAst
  case class RStringValue(value: String) extends RValue
  case class RIntValue(value: Int) extends RValue

  sealed trait ListAst
  case class ListField(name: String, next: List[ListField], values: List[ListValue]) extends ListAst
  sealed trait ListValue extends ListAst
  case class ListIntValue(value: Int) extends ListValue
}
