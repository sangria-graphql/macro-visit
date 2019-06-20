package sangria.visitor

import org.scalatest.{Matchers, WordSpec}
import sangria.visitor.util.StringMatchers

import scala.collection.immutable.Vector

class SimpleVisitorSpec extends WordSpec with Matchers with StringMatchers {
  sealed trait Ast

  case class Vertex(name: String, edges: List[Edge], fields: Vector[Field] = Vector.empty) extends Ast
  case class Edge(weight: Int, toVertex: Vertex) extends Ast
  case class Field(name: String, value: Option[Value]) extends Ast

  sealed trait Value extends Ast

  case class StringValue(value: String) extends Value
  case class IntValue(value: Int) extends Value

  "Visitor when used with GraphQL AST" should {
    "traverse and transform AST" in {
      val graph =
        Vertex("start", List(
          Edge(1, Vertex("colors", List(
            Edge(2, Vertex("RED", Nil, Vector(
              Field("intensity", Some(IntValue(123))),
              Field("hex", Some(StringValue("#FF0000")))
            ))),
            Edge(100, Vertex("GREEN", Nil, Vector(
              Field("hex", Some(StringValue("#00FF00")))
            )))
          ))),
          Edge(42, Vertex("books", List(
            Edge(1, Vertex("The Hobbit", Nil, Vector(
              Field("pages", Some(IntValue(320)))
            )))
          )))
        ))

      val res = visit[Ast](graph,
        Visit[Field](f => if (f.name == "hex") VisitorCommand.Delete else VisitorCommand.Continue),
        Visit[IntValue](v => VisitorCommand.Transform(IntValue(v.value + 1))))

      res should be(
        Vertex("start", List(
          Edge(1, Vertex("colors", List(
            Edge(2,Vertex("RED", Nil, Vector(
              Field("intensity", Some(IntValue(124)))))),
            Edge(100, Vertex("GREEN", Nil, Vector.empty))), Vector.empty)),
          Edge(42, Vertex("books", List(
            Edge(1, Vertex("The Hobbit", Nil, Vector(
              Field("pages", Some(IntValue(321))))))),
            Vector.empty))),
          Vector.empty))
    }
  }
}
