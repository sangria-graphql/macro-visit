package sangria.visitor

import org.parboiled2.Position
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.ast._
import sangria.macros._
import sangria.renderer.QueryRenderer
import sangria.visitor.util.StringMatchers

import scala.collection.immutable.Vector

class GraphQLVisitorSpec extends AnyWordSpec with Matchers with StringMatchers {
  "Visitor when used with GraphQL AST" should {
    "traverse and transform AST" in {
      val doc =
        graphql"""
          query Foo {
            # first field
            person(id: String, filters: [{firstName1: "Bob"}, {lastName1: "Doe"}]) {
              ...Names

              interests(first: 10) {
                name
                ranks(filter: {f1: "test"})
              }
            }
          }

          fragment Names on Person {
            firstName
            lastName
          }
        """

      var fields = Vector.empty[Field]
      var inputFields = Vector.empty[ObjectField]

      val enterField = (field: Field) => {
        fields = fields :+ field

        field.name match {
          case "firstName" =>
            VisitorCommand.Transform(field.copy(comments = Vector(Comment("Test comment"))))
          case "interests" =>
            VisitorCommand.Skip
          case _ =>
            VisitorCommand.Continue
        }
      }

      val leaveInputField = (field: ObjectField) => {
        inputFields = inputFields :+ field

        VisitorCommand.Continue
      }

      val res = visit[AstNode](
        doc,
        Visit[Field](enterField),
        Visit[ObjectField](enter = _ => VisitorCommand.Continue, leave = leaveInputField),
        VisitAnyFieldByName[Document, Option[Position]](
          "position",
          (_, _) => VisitorCommand.Transform(None))
      )

      QueryRenderer.renderPretty(res) should equal("""query Foo {
          |  # first field
          |  person(id: String, filters: [{firstName1: "Bob"}, {lastName1: "Doe"}]) {
          |    ...Names
          |    interests(first: 10) {
          |      name
          |      ranks(filter: {f1: "test"})
          |    }
          |  }
          |}
          |
          |fragment Names on Person {
          |  # Test comment
          |  firstName
          |  lastName
          |}""".stripMargin)(after.being(strippedOfCarriageReturns))

      fields.map(_.name) should be(Vector("person", "interests", "firstName", "lastName"))
      inputFields.map(_.name) should be(Vector("firstName1", "lastName1"))
    }
  }
}
