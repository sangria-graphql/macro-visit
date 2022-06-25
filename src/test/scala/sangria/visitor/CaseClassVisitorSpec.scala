package sangria.visitor

import sangria.visitor.util.StringMatchers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CaseClassVisitorSpec extends AnyWordSpec with Matchers with StringMatchers {
  import CaseClassVisitorSpec._

  "visiting a case class" should {
    "allow to change properties" in {
      val p = Person("Luke")
      val res = visit[Person](
        p,
        Visit[Person](p => VisitorCommand.Transform(p.copy(name = "Luc")))
      )
      res should be(Person("Luc"))
    }

    "allow to change properties when using a base trait" in {
      val p = Person("Luke")
      val res = visit[Ast](
        p,
        Visit[Person](p => VisitorCommand.Transform(p.copy(name = "Luc")))
      )
      res should be(Person("Luc"))
    }

    "stop changes" in {
      val p = Person("Luke")
      val res = visit[Person](
        p,
        Visit[Person](p => VisitorCommand.Break)
      )
      res should be(Person("Luke"))
    }
  }

}

object CaseClassVisitorSpec {
  sealed trait Ast
  case class Person(name: String) extends Ast
}
