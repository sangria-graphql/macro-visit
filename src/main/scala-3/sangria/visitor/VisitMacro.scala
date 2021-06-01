package sangria.visitor

import scala.quoted._
import scala.compiletime._
import scala.deriving._

object VisitMacro {
  given [T](using FromExpr[T]): FromExpr[Transformer[_ <: T]] with {
    def unapply(x: Expr[Transformer[_ <: T]])(using Quotes): Option[Transformer[_ <: T]] =
      x match {
        case _ => None
      }
  }

  def visitCode2[T](rootNode: Expr[T], transformations: Expr[Seq[Int]])(using Quotes): Expr[T] = {
    import quotes.reflect._

    val tree: Term = transformations.asTerm
//    println(transformations)
    println(rootNode.asTerm)
//    println(rootNode.show)

    val transformationValues = transformations match {
      case Varargs(transformationExprs) =>
        val numbers: Seq[Int] = transformationExprs.map(_.valueOrError)
        println("Varargs " + numbers)
        println(transformationExprs)
//        transformationExprs.map(_.valueOrError)
      case _ =>
        println("error")
//        error("Mode must be a known value but got: " + codeOf(transformations))
    }

    transformations match {
      case '{ $x: Seq[Int] } => println("Seq[Int] " + x)
      case _ => println("does not know")
    }
    rootNode
  }

  def visitCode[T](rootNode: Expr[T], transformations: Expr[Seq[Transformer[_ <: T]]])(using tt: Type[T])(using Quotes): Expr[T] = {
    import quotes.reflect._

    val tpe: TypeRepr = TypeRepr.of[T]
    println(tpe)

//    tpe match {
//      case '{ $mirrorExpr : Mirror.Sum { type MirroredLabel = label } } => Type.valueOfConstant[label] // Option[String] }
//    }



    val tree: Term = transformations.asTerm
//    println(transformations)
//    println(rootNode.asTerm)
//    println(rootNode.show)

    // collectKnownSubtypes of T


    val transformationValues = transformations match {
      case Varargs(transformationExprs) =>
        val validatedConfig = validateTransformations(transformationExprs)
        val errors: Seq[String] = validatedConfig.collect { case Left(error) => error }
        println("errors: " + errors)

        if (errors.nonEmpty) reportErrors(errors)
        else {
          val validConfig: Seq[MacroTransformer] = validatedConfig.collect { case Right(cfg) => cfg }

        }
//        transformationExprs.foreach(valueOfTransformer)
        println("Varargs")
        println(transformationExprs)
//        transformationExprs.map(_.valueOrError)
      case _ =>
        println("error")
//        error("Mode must be a known value but got: " + codeOf(transformations))
    }

    transformations match {
      case '{ $t: Seq[sangria.visitor.Transformer[_ <: T]] } => println("seq of transformer")
      case _ => println("does not know")
    }
    rootNode
  }

  private def reportErrors(errors: Seq[String]) = {
    require(errors.nonEmpty)
    throw new Exception(errors.mkString("\n"))
  }

  private def validateTransformations[T: Type](transformations: Seq[Expr[Transformer[_]]])(using Quotes): Seq[Either[String, MacroTransformer]] =
    transformations.map {
      case '{
        type u <: T
        Visit.apply[`u`](
          $enter: `u` => VisitorCommand,
          $leave: `u` => VisitorCommand)
        } => Right(MacroVisit(enter, leave))
      case '{ $x: sangria.visitor.VisitAnyField[_, _] } => Left("VisitAnyField")
      case '{ $x: sangria.visitor.VisitAnyFieldByName[_, _] } => Left("VisitAnyFieldByName")
      case _ => Left("does not know")
    }
  def valueOfTransformer(t: Expr[Transformer[_]])(using Quotes): Unit =
    t match {
      case '{ $x: sangria.visitor.Visit[_] } => println("Visit " + x)
      case '{ $x: sangria.visitor.VisitAnyField[_, _] } => println("VisitAnyField")
      case '{ $x: sangria.visitor.VisitAnyFieldByName[_, _] } => println("VisitAnyFieldByName")
      case _ => println("does not know")
    }

  sealed trait MacroTransformer
  case class MacroVisit[T](enter: Expr[T => VisitorCommand], leave: Expr[T => VisitorCommand]) extends MacroTransformer

}
