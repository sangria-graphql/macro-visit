package sangria.visitor

import scala.quoted._
import scala.compiletime._
import scala.deriving._

object VisitMacro {

  private def collectKnownSubtypes[T](using
      quotes: Quotes,
      tpe: Type[T]): Set[quotes.reflect.Symbol] = {
    import quotes.reflect._
    val children: List[Symbol] = TypeTree.of[T].symbol.children
    children.flatMap { s =>
      val flags = s.flags
      if (flags.is(Flags.Sealed) && flags.is(Flags.Trait)) {
        println("sealed trait " + s)
        // TODO: fetch children of sealed trait
        // println(boxTpe.memberType(s))
        // val childTpe: TypeTree = TypeIdent(s)
        // val childType: TypeRepr = childTpe.tpe
        // val tt = Type.of(childType)
        // collectKnownSubtypes[tt.Underlying](quotes, childType)
        s :: Nil
      } else {
        s :: Nil
      }
    }
    children.toSet
  }

  def visitCode[T](rootNode: Expr[T], transformations: Expr[Seq[Transformer[_ <: T]]])(using
      tt: Type[T])(using Quotes): Expr[T] = {
    import quotes.reflect._

    val children = collectKnownSubtypes[T]

    val transformationValues: Seq[MacroTransformer] = transformations match {
      case Varargs(transformationExprs) =>
        val validatedConfig = validateTransformations(transformationExprs)
        val errors: Seq[String] = validatedConfig.collect { case Left(error) => error }
        println("errors: " + errors)

        if (errors.nonEmpty) reportErrors(errors)
        else {
          val validConfig: Seq[MacroTransformer] = validatedConfig.collect { case Right(cfg) =>
            cfg
          }
          validConfig
        }
      case _ =>
        reportErrors("error" :: Nil)
    }

    val tpe: TypeRepr = TypeRepr.of[T]
    println(tpe.typeSymbol)

    tpe.typeSymbol

    generateTraversal(rootNode, transformationValues)
  }

  private def reportErrors(errors: Seq[String])(using quotes: Quotes): Nothing = {
    require(errors.nonEmpty)
    val msg = errors.mkString("\n")
    quotes.reflect.report.error(msg)
    throw new Exception(msg)
  }

  private def validateTransformations[T: Type](transformations: Seq[Expr[Transformer[_]]])(using
      quotes: Quotes): Seq[Either[String, MacroTransformer]] =
    transformations.map {
      case '{
            type u <: T
            Visit[`u`]($enter: `u` => VisitorCommand, $leave: `u` => VisitorCommand)
          } =>
        Right(MacroVisit(quotes)(quotes.reflect.TypeRepr.of[u], enter, leave))
      case '{ $x: sangria.visitor.VisitAnyField[_, _] } => Left("VisitAnyField")
      case '{ $x: sangria.visitor.VisitAnyFieldByName[_, _] } => Left("VisitAnyFieldByName")
      case _ => Left("does not know")
    }

  sealed trait MacroTransformer
  case class MacroVisit[T](quotes: Quotes)(
      val typ: quotes.reflect.TypeRepr,
      val enter: Expr[T => VisitorCommand],
      val leave: Expr[T => VisitorCommand])
      extends MacroTransformer

  def generateTraversal[T](
      node: Expr[T],
      transformationValues: Seq[VisitMacro.MacroTransformer]
  )(using quotes: Quotes, tpe: Type[T]): Expr[T] = {
    import quotes.reflect._
    val t1 = transformationValues.head.asInstanceOf[MacroVisit[T]]

    val nodeTypeSymbol = node match {
      case '{
            type u <: T
            $x: `u`
          } =>
        quotes.reflect.TypeRepr.of[u].typeSymbol
      case _ => TypeRepr.of[T].typeSymbol
    }

    val transformationCode: Option[Expr[VisitorCommand]] =
      if (t1.typ.typeSymbol == nodeTypeSymbol) {
        Some('{
          ${ t1.enter }.apply($node)
        })
      } else None

    '{
      val rootNode = $node
      var stack = _root_.sangria.visitor.VisitorStack.initial[tpe.Underlying](rootNode)
      var breakMode = false
      var nestedUpdated: Option[tpe.Underlying] = None
      var nestedDeleted = false

      // while (stack != null)
      // stack.node match {
      //   case ..$cases
      // }
      // stack = null

      ${
        transformationCode.match {
          case None => '{ () }
          case Some(t) =>
            '{
              ($t: _root_.sangria.visitor.VisitorCommand) match {
                case _root_.sangria.visitor.VisitorCommand.Break => ()
                case t: _root_.sangria.visitor.VisitorCommand.Transform[T] =>
                  nestedUpdated = Some(t.newValue)
                case _ => ()
              }
            }
        }
      }

      nestedUpdated.getOrElse(rootNode)
    }
  }
}
