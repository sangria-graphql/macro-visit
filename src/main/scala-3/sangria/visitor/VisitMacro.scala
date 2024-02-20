package sangria.visitor

import scala.quoted._
import scala.compiletime._
import scala.deriving._
import scala.annotation.meta.field

import scala.collection.mutable.{Map => MutableMap}

object VisitMacro {
  def visitCode[T](rootNode: Expr[T], transformations: Expr[Seq[Transformer[_ <: T]]])(using
      tt: Type[T])(using Quotes): Expr[T] =
    new VisitMacro().visitCode[T](rootNode, transformations)
}

class VisitMacro(using val globalQuotes: Quotes) {
  import globalQuotes.reflect._

  private def collectKnownSubtypes[T](using tpe: Type[T]): Set[quotes.reflect.Symbol] = {
    import quotes.reflect._
    val parent: Symbol = TypeTree.of[T].symbol
    val children: List[Symbol] = parent.children
    val parentFlags = parent.flags
    if (parentFlags.is(Flags.Case))
      Set(parent)
    else
      children.flatMap { s =>
        val flags = s.flags
        if (flags.is(Flags.Sealed) && flags.is(Flags.Trait)) {
          val childTpe: TypeTree = TypeIdent(s)
          val childType: TypeRepr = childTpe.tpe
          val subtypes = childType.asType match
            case '[t] => collectKnownSubtypes[t]
          subtypes
        } else {
          s :: Nil
        }
      }.toSet
  }

  def visitCode[T](rootNode: Expr[T], transformations: Expr[Seq[Transformer[_ <: T]]])(using
      tt: Type[T]): Expr[T] = {
    import quotes.reflect._

    val subclasses = collectKnownSubtypes[T]

    val transformationValues: Seq[MacroTransformer] = transformations match {
      case Varargs(transformationExprs) =>
        val validatedConfig = validateTransformations(transformationExprs)
        val errors: Seq[String] = validatedConfig.collect { case Left(error) => error }

        if (errors.nonEmpty) reportErrors(errors)
        else {
          val validConfig: Seq[MacroTransformer] = validatedConfig.collect { case Right(cfg) =>
            cfg
          }
          validConfig
        }
      case _ =>
        reportErrors("Nonempty transformations argument expected" :: Nil)
    }

    generateTraversal(rootNode, subclasses, transformationValues)
  }

  private def reportErrors(errors: Seq[String]): Nothing = {
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
        Right(MacroVisit[u](TypeRepr.of[u], Type.of[u], enter, leave))
      case '{
            type u <: T
            type s
            VisitAnyField[`u`, `s`]($fn: (`u`, `s`) => VisitorCommand)
          } =>
        Right(
          MacroVisitAnyField[u, s](
            TypeRepr.of[u],
            Type.of[u],
            TypeRepr.of[s],
            Type.of[s],
            fn,
            None))
      case '{
            type u <: T
            type s
            VisitAnyFieldByName[`u`, `s`]($fieldName: String, $fn: (`u`, `s`) => VisitorCommand)
          } =>
        val strFieldName = fieldName.valueOrAbort
        Right(
          MacroVisitAnyField[u, s](
            TypeRepr.of[u],
            Type.of[u],
            TypeRepr.of[s],
            Type.of[s],
            fn,
            Some(strFieldName)))
      case _ => Left("does not know")
    }

  def generateTraversal[T](using tpe: Type[T])(
      node: Expr[T],
      subclasses: Set[Symbol],
      transformationValues: Seq[MacroTransformer]
  ): Expr[T] = {
    val specials = transformationValues.collect { case s: MacroVisitAnyField[mt, ms] => s }

    val infos =
      subclasses.map { cls =>
        val clsTypeRepr = TypeIdent(cls).tpe
        val fieldMembers = findKnownMembers(cls, specials)
        val (knownMembers: Seq[KnownMember], defaultFields: Seq[DefaultField]) =
          fieldMembers.partitionMap {
            case m: KnownMember => Left(m)
            case m: DefaultField => Right(m)
          }

        clsTypeRepr.asType match {
          case '[t] =>
            def genNameWithPrefix(prefix: String) =
              s"${prefix}_${cls.fullName}".flatMap {
                case '.' => "_"
                case other => other.toString
              }
            val applyEditsName = genNameWithPrefix("applyEdits")
            val onEnterName = genNameWithPrefix("onEnter")
            val onLeaveName = genNameWithPrefix("onLeave")

            /** We wrap access with a typed anonymous function, giving us an easy access to a method
              * via an Expr from a typed scala 3 quotation.
              */
            def genAnonfun[A: Type, B: Type](targetDefDef: DefDef): Expr[A => B] = {
              val anonfunSymbol = Symbol.newMethod(
                Symbol.spliceOwner,
                "$anonfun",
                MethodType(List("stack"))(_ => List(TypeRepr.of[A]), _ => TypeRepr.of[B])
              )
              val anonfunDef = DefDef(
                anonfunSymbol,
                (params: List[List[Tree]]) =>
                  Some(
                    Apply(
                      Ref(targetDefDef.symbol),
                      List(params.head.head.asExprOf[VisitorStack[T]].asTerm))
                  )
              )

              Block(List(anonfunDef), Closure(Ref(anonfunSymbol), None))
                .asExprOf[A => B]
            }

            val onApplyEditsDef =
              generateOnApplyEdits[T, t](applyEditsName, clsTypeRepr, knownMembers, defaultFields)
            val applyEditsExpr = genAnonfun[VisitorStack[T], Option[T]](onApplyEditsDef)

            val onEnterDef =
              generateOnEnter[T](onEnterName, clsTypeRepr, transformationValues, knownMembers)
            val onEnterExpr = genAnonfun[VisitorStack[T], VisitorControlCommand](onEnterDef)

            val onLeaveDef = generateOnLeave[T](onLeaveName, clsTypeRepr, transformationValues)
            val onLeaveExpr = genAnonfun[VisitorStack[T], VisitorControlCommand](onLeaveDef)

            VisitInfo[T, t](
              summon[Type[t]],
              applyEditsExpr,
              onApplyEditsDef,
              onEnterExpr,
              onEnterDef,
              onLeaveExpr,
              onLeaveDef,
              knownMembers
            )
        }
      }

    case class IsInstanceOfIf[M](ifCondType: Type[M], ifThen: Expr[VisitorStack[T]])

    def genIfs(using q: Quotes)(
        rootNode: Expr[T],
        stack: Expr[VisitorStack[T]],
        breakMode: Expr[Boolean],
        nestedUpdated: Expr[Option[T]],
        nestedDeleted: Expr[Boolean]) =
      infos.toList.reverse.map { info =>
        import q.reflect._

        val membersNoSpecial = info.members.filter(_.memberType != MemberType.Special)

        def getMemberSize[M](using q1: Quotes)(using Type[M])(m: KnownMember, currNode: Expr[M]) = {
          import q1.reflect._

          m.memberType match {
            case MemberType.Seq | MemberType.List | MemberType.Vector =>
              Select.unique(Select.unique(currNode.asTerm, m.member.name), "size").asExprOf[Int]
            case MemberType.Option =>
              '{
                if (${ unsafeSelectByName[M, Option[_]](currNode, m.member.name) }.isEmpty) 0 else 1
              }
            case _ => '{ 1 }
          }
        }

        def nextMember(using q1: Quotes)(m: KnownMember): Expr[String] = {
          import q1.reflect._
          val next = membersNoSpecial.dropWhile(_.member.name != m.member.name).drop(1).headOption

          next match {
            case Some(nm) => Expr(nm.member.name)
            case None => '{ null }
          }
        }

        def nextMemberSize[M](using Type[M])(m: KnownMember, currNode: Expr[M]) = {
          val next = membersNoSpecial.dropWhile(_.member.name != m.member.name).drop(1).headOption

          next match {
            case Some(nm) => getMemberSize(nm, currNode)
            case None => '{ -1 }
          }
        }

        def memberNode[M](using q1: Quotes)(using
            Type[M])(m: KnownMember, currNode: Expr[M], memberIndex: Expr[Int]) = {
          import q1.reflect._
          m.memberType match {
            case MemberType.Seq | MemberType.List | MemberType.Vector =>
              Apply(
                Select.unique(Select.unique(currNode.asTerm, m.member.name), "apply"),
                List(memberIndex.asTerm)).asExprOf[T]
            case MemberType.Option =>
              Select.unique(Select.unique(currNode.asTerm, m.member.name), "get").asExprOf[T]
            case MemberType.Normal =>
              Select.unique(currNode.asTerm, m.member.name).asExprOf[T]
            case MemberType.Special => '{ ??? }
          }
        }

        def selectNextMemberCase[M](using q1: Quotes)(using Type[M])(
            isLeaving: Expr[Boolean],
            memberNod: Expr[Option[T]],
            currMember: Expr[String],
            memberIndex: Expr[Int],
            memberSize: Expr[Int],
            currNode: Expr[M]) =
          membersNoSpecial.zipWithIndex.map { case (m, idx) =>
            import q1.reflect._

            val rhs =
              '{
                if ($memberIndex <= $memberSize - 1) {
                  ${
                    unsafeAssign(
                      memberNod,
                      '{ Option(${ memberNode[M](m, currNode, memberIndex) }.asInstanceOf[T]) })
                  }
                } else {
                  ${ unsafeAssign(memberNod, Expr(None)) }
                  ${ unsafeAssign(currMember, nextMember(m)) }
                  ${ unsafeAssign(memberIndex, Expr(-1)) }

                  ${ unsafeAssign(memberSize, nextMemberSize[M](m, currNode)) }

                  if ($currMember == null) {
                    ${ unsafeAssign(isLeaving, Expr(true)) }
                  }
                }
              }
            CaseDef(Expr(m.member.name).asTerm, None, rhs.asTerm)
          }

        def selectNextMember[M](using q1: Quotes)(using Type[M])(
            isLeaving: Expr[Boolean],
            memberNod: Expr[Option[T]],
            currMember: Expr[String],
            memberIndex: Expr[Int],
            memberSize: Expr[Int],
            currNode: Expr[M]): Expr[Unit] = {
          import q1.reflect._
          Match(
            currMember.asTerm,
            selectNextMemberCase(
              isLeaving,
              memberNod,
              currMember,
              memberIndex,
              memberSize,
              currNode).toList
          ).asExprOf[Unit]
        }

        info.tpe match {
          case '[t] =>
            val logic = {
              if (membersNoSpecial.nonEmpty) {
                '{
                  var isLeaving = false
                  var memberNod: Option[T] = None
                  var currMember = $stack.currMember
                  var memberIndex = $stack.memberIndex
                  var memberSize = $stack.memberSize

                  if ($nestedUpdated != None || $nestedDeleted) {
                    if ($stack.edits == null)
                      $stack.edits = scala.collection.mutable.Map()

                    $stack.edits.get(currMember) match {
                      case Some(me) => me
                      case None =>
                        $stack.edits(currMember) = scala.collection.mutable.Map[Int, Option[T]]()
                    }

                    if ($nestedDeleted) {
                      $stack.edits(currMember)(memberIndex) = null
                    } else if ($nestedUpdated != None) {
                      $stack.edits(currMember)(memberIndex) = $nestedUpdated
                    }

                    ${ unsafeAssign(nestedDeleted, '{ false }) }
                    ${ unsafeAssign(nestedUpdated, '{ None }) }
                  }
                  if (! $breakMode) {
                    var enterResult: VisitorControlCommand = VisitorCommand.Continue

                    var currNode = $stack.node.get.asInstanceOf[t]

                    if (currMember == null) {
                      enterResult = ${ info.onEnterExpr }($stack)
                      currMember = ${ Expr(membersNoSpecial.head.member.name) }
                      memberSize = ${ getMemberSize[t](membersNoSpecial.head, 'currNode) }
                    }

                    enterResult match {
                      case VisitorCommand.Continue =>
                        while ({
                          memberIndex = memberIndex + 1
                          ${
                            selectNextMember[t](
                              'isLeaving,
                              'memberNod,
                              'currMember,
                              'memberIndex,
                              'memberSize,
                              'currNode)
                          }
                          memberNod == None && !isLeaving
                        }) ()

                      case VisitorCommand.Skip =>
                        isLeaving = true

                      case VisitorCommand.Break =>
                        ${ unsafeAssign(breakMode, '{ true }) }

                        isLeaving = true
                    }
                  } else {
                    isLeaving = true
                  }

                  $stack.currMember = currMember
                  $stack.memberIndex = memberIndex
                  $stack.memberSize = memberSize
                  if (isLeaving) {
                    if ($stack.edits != null && $stack.edits.nonEmpty || $stack.specialEdits != null && $stack.specialEdits.nonEmpty)
                      $stack.node = ${ info.applyEditsExpr }($stack).asInstanceOf[Option[T]]
                    val leaveResult = ${ info.onLeaveExpr }($stack)

                    if (leaveResult == VisitorCommand.Break)
                      ${ unsafeAssign(breakMode, '{ true }) }

                    ${ unsafeAssign(nestedUpdated, '{ if ($stack.updated) $stack.node else None }) }

                    ${ unsafeAssign(nestedDeleted, '{ $stack.deleted }) }

                    $stack.prev
                  } else {
                    var next = $stack.next

                    if (next != null) {
                      next.node = memberNod.asInstanceOf[Option[T]]
                      next.updated = false
                      next.deleted = false
                      next.currMember = null
                      next.memberIndex = -1
                      next.memberSize = -1
                      next.edits = null
                      next.specialEdits = null
                    } else {
                      next = new VisitorStack[T](
                        memberNod.asInstanceOf[Option[T]],
                        false,
                        false,
                        null,
                        -1,
                        -1,
                        null,
                        null,
                        $stack,
                        null)
                    }

                    next
                  }
                }
              } else {
                '{
                  val enterResult = ${ info.onEnterExpr }($stack)

                  if (enterResult == VisitorCommand.Break)
                    ${ unsafeAssign(breakMode, '{ true }) }

                  if (${ stack }.specialEdits != null && ${ stack }.specialEdits.nonEmpty)
                    ${ stack }.node = ${ info.applyEditsExpr }($stack).asInstanceOf[Option[T]]

                  val leaveResult = ${ info.onLeaveExpr }($stack)

                  if (leaveResult == VisitorCommand.Break)
                    ${ unsafeAssign(breakMode, '{ true }) }

                  ${
                    unsafeAssign(
                      nestedUpdated,
                      '{ if (${ stack }.updated) ${ stack }.node else None })
                  }

                  ${ unsafeAssign(nestedDeleted, '{ ${ stack }.deleted }) }

                  $stack.prev
                }
              }
            }

            IsInstanceOfIf[t](Type.of[t], logic)
        }
      }

    def genMatch(using q: Quotes)(
        rootNode: Expr[T],
        stack: Expr[VisitorStack[T]],
        breakMode: Expr[Boolean],
        nestedUpdated: Expr[Option[T]],
        nestedDeleted: Expr[Boolean]): Expr[VisitorStack[T]] = {
      import q.reflect._
      val isInstanceOfIfs = genIfs(rootNode, stack, breakMode, nestedUpdated, nestedDeleted)
      val elseThrow = '{ throw new IllegalStateException("Match") }.asTerm
      isInstanceOfIfs
        .foldRight(elseThrow) { case (IsInstanceOfIf(ifType, ifThen), elseIf) =>
          ifType match {
            case '[t] =>
              If(
                TypeApply(
                  Select.unique('{ $stack.node.getOrElse(null) }.asTerm, "isInstanceOf"),
                  List(TypeTree.of[t])),
                ifThen.asTerm,
                elseIf)
          }
        }
        .asExprOf[VisitorStack[T]]
    }

    val applyEditsBlock = Block(
      infos.map(_.applyEditsDef).toList,
      '{ () }.asTerm
    )

    val mainCode = '{
      val rootNode = $node
      var stack = VisitorStack.initial[tpe.Underlying](rootNode)
      var breakMode = false
      var nestedUpdated: Option[tpe.Underlying] = None
      var nestedDeleted = false

      while ({
        stack = ${ genMatch('rootNode, 'stack, 'breakMode, 'nestedUpdated, 'nestedDeleted) }
        stack != null
      }) ()
      nestedUpdated.getOrElse(rootNode)
    }

    Block(
      infos.map(_.applyEditsDef).toList ++ infos.map(_.onEnterDef).toList ++ infos.map(
        _.onLeaveDef),
      mainCode.asTerm
    ).asExprOf[T]
  }

  private def generateOnEnter[T](using tpe: Type[T])(
      methodName: String,
      typeRepr: TypeRepr,
      tx: Seq[MacroTransformer],
      members: Seq[KnownMember]): DefDef = {
    def enterLogic(t: MacroTransformer): Expr[(
        (VisitorControlCommand, VisitorStack[T])) => (VisitorControlCommand, VisitorStack[T])] =
      t match {
        case visit: MacroVisit[mt] =>
          given Type[mt] = visit.trueType
          '{ (enterResult: VisitorControlCommand, stack: VisitorStack[T]) =>
            if (enterResult == VisitorCommand.Continue) {
              (${ visit.enter }(
                stack.node.getOrElse(null).asInstanceOf[mt]): VisitorCommand) match {
                case cc: VisitorControlCommand =>
                  (cc, stack)
                case tr: VisitorCommand.Transform[_] =>
                  stack.updated = true
                  stack.node = Option(tr.newValue.asInstanceOf[T])(tr.controlCommand, stack)
                case VisitorCommand.Delete =>
                  stack.deleted = true (VisitorCommand.Skip, stack)
                case VisitorCommand.DeleteAndBreak =>
                  stack.deleted = true (VisitorCommand.Break, stack)
              }
            } else {
              (enterResult, stack)
            }
          }

        case special: MacroVisitAnyField[mt, st] =>
          given Type[mt] = special.trueMatchType
          given Type[st] = special.trueSpecialType
          val specialMembers =
            members.filter(m =>
              m.memberType == MemberType.Special &&
                special.specialType <:< m.elemType &&
                special.fieldName.fold(true)(fn => m.member.name == fn))

          val specialCode: Seq[Expr[((VisitorControlCommand, VisitorStack[T])) => (
              VisitorControlCommand,
              VisitorStack[T])]] =
            specialMembers.map { sm =>
              '{ (enterResult: VisitorControlCommand, stack: VisitorStack[T]) =>
                if (enterResult == VisitorCommand.Continue) {
                  val actualNode = stack.node.getOrElse(null).asInstanceOf[mt]

                  (${ special.fn }(
                    actualNode,
                    ${
                      unsafeSelectByName[mt, st]('actualNode, sm.member.name)
                    }): VisitorCommand) match {
                    case cc: VisitorControlCommand =>
                      (cc, stack)
                    case tr: VisitorCommand.Transform[_] =>
                      if (stack.specialEdits == null)
                        stack.specialEdits = scala.collection.mutable.Map()

                      stack.specialEdits(${ Expr(sm.member.name) }) =
                        tr.newValue(tr.controlCommand, stack)
                    case VisitorCommand.Delete =>
                      if (stack.specialEdits == null)
                        stack.specialEdits = scala.collection.mutable.Map()

                      stack.specialEdits(${ Expr(sm.member.name) }) =
                        null (VisitorCommand.Skip, stack)
                    case VisitorCommand.DeleteAndBreak =>
                      if (stack.specialEdits == null)
                        stack.specialEdits = scala.collection.mutable.Map()

                      stack.specialEdits(${ Expr(sm.member.name) }) =
                        null (VisitorCommand.Break, stack)
                  }
                } else {
                  (enterResult, stack)
                }
              }
            }

          '{ (enterResult: VisitorControlCommand, stack: VisitorStack[T]) =>
            ${ flattenExprs[(VisitorControlCommand, VisitorStack[T])](specialCode) }(
              enterResult,
              stack)
          }

      }

    def body(stack: Term) = '{
      val enterResult: VisitorControlCommand = VisitorCommand.Continue

      ${
        flattenExprs[(VisitorControlCommand, VisitorStack[T])](
          transformersForType(typeRepr, tx).map(enterLogic))
      }(enterResult, ${ stack.asExprOf[VisitorStack[T]] })._1
    }

    val methodSymbol = Symbol.newMethod(
      Symbol.spliceOwner,
      methodName,
      MethodType(List("param"))(
        _ => List(TypeRepr.of[VisitorStack[T]]),
        _ => TypeRepr.of[VisitorControlCommand]
      )
    )

    DefDef(
      methodSymbol,
      { case List(List(paramTerm: Term)) => Some(body(paramTerm).asTerm.changeOwner(methodSymbol)) }
    )
  }

  private def generateOnLeave[T](using Type[T])(using
      quotes: Quotes)(methodName: String, typeRepr: TypeRepr, tx: Seq[MacroTransformer]) = {
    def leaveLogic(t: MacroTransformer): Expr[(
        (VisitorControlCommand, VisitorStack[T])) => (VisitorControlCommand, VisitorStack[T])] =
      t match {
        case visit: MacroVisit[mt] =>
          given Type[mt] = visit.trueType
          '{ (leaveResult: VisitorControlCommand, stack: VisitorStack[T]) =>
            if (leaveResult == VisitorCommand.Continue) {
              (${ visit.leave }(
                stack.node.getOrElse(null).asInstanceOf[mt]): VisitorCommand) match {
                case cc: VisitorControlCommand =>
                  (cc, stack)
                case tr: VisitorCommand.Transform[_] =>
                  stack.updated = true
                  stack.node = Option(tr.newValue.asInstanceOf[T])(tr.controlCommand, stack)
                case VisitorCommand.Delete =>
                  stack.deleted = true (VisitorCommand.Skip, stack)
                case VisitorCommand.DeleteAndBreak =>
                  stack.deleted = true (VisitorCommand.Break, stack)
              }
            } else (leaveResult, stack)
          }

        case special: MacroVisitAnyField[mt, ms] =>
          '{ (leaveResult, stack) => (leaveResult, stack) }
      }

    def body(stack: Term) = '{
      var leaveResult: VisitorControlCommand = VisitorCommand.Continue

      ${
        flattenExprs[(VisitorControlCommand, VisitorStack[T])](
          transformersForType(typeRepr, tx).map(leaveLogic))
      }(leaveResult, ${ stack.asExprOf[VisitorStack[T]] })._1
    }

    val methodSymbol = Symbol.newMethod(
      Symbol.spliceOwner,
      methodName,
      MethodType(List("param"))(
        _ => List(TypeRepr.of[VisitorStack[T]]),
        _ => TypeRepr.of[VisitorControlCommand]
      )
    )

    DefDef(
      methodSymbol,
      { case List(List(paramTerm: Term)) => Some(body(paramTerm).asTerm.changeOwner(methodSymbol)) }
    )
  }

  private def generateOnApplyEdits[T, M](using Type[T], Type[M])(
      methodName: String,
      infoTypeRepr: TypeRepr,
      members: Seq[KnownMember],
      defaultFields: Seq[DefaultField]): DefDef = {

    def applyActualMemberEdits(using
        q: Quotes)(m: KnownMember, edits: Expr[MutableMap[Int, Option[T]]], origNode: Expr[M]) =
      (m.elemType.asType, m.fullType) match
        case ('[t], '[f]) =>
          m.memberType match {
            case MemberType.Normal =>
              '{
                if $edits(0) == null then ${ unsafeSelectByName[M, t](origNode, m.member.name) }
                else $edits(0).orNull.asInstanceOf[t]
              }
            case MemberType.Option =>
              '{
                if ($edits(0) == null || $edits(0).getOrElse(null) == null)
                  None
                else
                  Some($edits(0).orNull.asInstanceOf[t])
              }

            case MemberType.List | MemberType.Vector | MemberType.Seq =>
              '{
                val orig = ${
                  m.memberType match
                    case MemberType.List => unsafeSelectByName[M, List[t]](origNode, m.member.name)
                    case MemberType.Vector =>
                      unsafeSelectByName[M, Vector[t]](origNode, m.member.name)
                    case MemberType.Seq => unsafeSelectByName[M, Seq[t]](origNode, m.member.name)
                    case _ =>
                      throw new IllegalStateException(
                        "Error in macro expansion. Collection MemberType expected")
                }
                val builder = new scala.collection.immutable.VectorBuilder[t]
                var idx = 0

                while (idx < orig.size) {
                  $edits.get(idx) match {
                    case Some(None) | Some(null) => // do nothing - element is deleted
                    case Some(Some(elem)) => builder += elem.asInstanceOf[t]
                    case None => builder += orig(idx)
                  }

                  idx += 1
                }

                ${
                  m.memberType match {
                    case MemberType.List => '{ builder.result().toList }
                    case _ => '{ builder.result() }
                  }
                }
              }

            case MemberType.Special => '{ ??? }

          }

    def applyMemberEdits(using
        q: Quotes)(m: KnownMember, stack: Expr[VisitorStack[T]], origNode: Expr[M]) =
      if (m.memberType != MemberType.Special) {
        import q.reflect._

        m.fullType match
          case '[t] =>
            '{
              if (${ stack }.edits != null)
                ${ stack }.edits.get(${ Expr(m.member.name) }) match {
                  case Some(edits) if edits != null && edits.nonEmpty =>
                    ${ applyActualMemberEdits(m, 'edits, origNode) }

                  case _ =>
                    ${ unsafeSelectByName[M, t](origNode, m.member.name) }
                }
              else ${ unsafeSelectByName[M, t](origNode, m.member.name) }
            }
      } else
        m.fullType match
          case '[t] =>
            '{
              if ($stack.specialEdits != null)
                $stack.specialEdits.get(${ Expr(m.member.name) }) match {
                  case Some(edit) if edit != null =>
                    edit.asInstanceOf[t]

                  case _ =>
                    ${ unsafeSelectByName[M, t](origNode, m.member.name) }
                }
              else ${ unsafeSelectByName[M, t](origNode, m.member.name) }
            }

    def generateModifiedOrigNode(using
        q: Quotes)(origNode: Expr[M], stack: Expr[VisitorStack[T]]): Expr[M] = {
      import q.reflect._
      val copyList = members
        .map(m => NamedArg(m.member.name, applyMemberEdits(m, stack, origNode).asTerm))
        .toList
      val defaultList = defaultFields.map(f =>
        NamedArg(
          f.member.name,
          Select(
            origNode.asTerm,
            origNode.asTerm.tpe.typeSymbol
              .methodMember("copy$default$" + (f.idx + 1).toString)
              .head)))

      if (!copyList.isEmpty)
        Apply(
          Select(origNode.asTerm, origNode.asTerm.tpe.typeSymbol.methodMember("copy").head),
          copyList ++ defaultList).asExprOf[M]
      else
        origNode
    }

    def body(stack: Term) = '{
      ${ stack.asExprOf[VisitorStack[T]] }.updated = true

      val origNodeMaybe = ${ stack.asExprOf[VisitorStack[T]] }.node.asInstanceOf[Option[M]]

      origNodeMaybe.map(origNode =>
        ${ generateModifiedOrigNode('origNode, stack.asExprOf[VisitorStack[T]]) }.asInstanceOf[T])
    }

    val methodSymbol = Symbol.newMethod(
      Symbol.spliceOwner,
      methodName,
      MethodType(List("param"))(
        _ => List(TypeRepr.of[VisitorStack[T]]),
        _ => TypeRepr.of[Option[T]]
      )
    )

    DefDef(
      methodSymbol,
      { case List(List(paramTerm: Term)) => Some(body(paramTerm).asTerm.changeOwner(methodSymbol)) }
    )
  }

  def flattenExprs[T](using Type[T], Quotes)(exprs: Seq[Expr[T => T]]): Expr[T => T] =
    if (exprs.isEmpty) '{ (a: T) => a }
    else '{ (t: T) => ${ exprs.head }(${ flattenExprs(exprs.tail) }(t)) }

  def transformersForType(tpe: TypeRepr, tx: Seq[MacroTransformer]) =
    tx.filter(t => tpe <:< t.matchType)

  private def findMemberType(
      baseType: TypeRepr,
      name: String,
      fieldType: TypeRepr,
      specials: Seq[MacroVisitAnyField[?, ?]]): Option[(TypeRepr, MemberType)] =
    fieldType.asType match {
      case '[List[t]] if fieldType.typeArgs.nonEmpty && fieldType.typeArgs.head <:< baseType =>
        Some(fieldType.typeArgs.head -> MemberType.List)
      case '[Vector[t]] if fieldType.typeArgs.nonEmpty && fieldType.typeArgs.head <:< baseType =>
        Some(fieldType.typeArgs.head -> MemberType.Vector)
      case '[Seq[t]] if fieldType.typeArgs.nonEmpty && fieldType.typeArgs.head <:< baseType =>
        Some(fieldType.typeArgs.head -> MemberType.Seq)
      case '[Option[t]] if fieldType.typeArgs.nonEmpty && fieldType.typeArgs.head <:< baseType =>
        Some(fieldType.typeArgs.head -> MemberType.Option)
      case '[t] if fieldType <:< baseType =>
        Some(fieldType -> MemberType.Normal)
      case '[t]
          if specials.exists(s =>
            fieldType <:< s.specialType && s.fieldName.fold(true)(fn => name == fn)) =>
        Some(fieldType -> MemberType.Special)
      case '[t] => None
    }

  private def findKnownMembers[T](using baseTpe: Type[T])(
      symbol: Symbol,
      specials: Seq[MacroVisitAnyField[?, ?]]
  ): Seq[MemberField] = {
    val baseTypeTree = TypeTree.of[T]
    val baseTypeRepr = baseTypeTree.tpe
    val baseSymbol: Symbol = baseTypeTree.symbol
    symbol.caseFields.zipWithIndex
      .map { (m, idx) =>
        m.tree match {
          case ValDef(_, typeTree, _) =>
            val caseFieldType: TypeRepr = typeTree.tpe
            val memberType = findMemberType(baseTypeRepr, m.name, caseFieldType, specials)
            memberType
              .map { case (et, mt) =>
                KnownMember(baseTypeRepr, m, mt, et)
              }
              .getOrElse(DefaultField(idx, m))
          case other =>
            throw new Exception("ERROR: unhandled tree: " + other)
        }
      }
      .toList
      .reverse
  }

  private case class VisitInfo[T, M](
      tpe: Type[M],
      applyEditsExpr: Expr[VisitorStack[T] => Option[T]],
      applyEditsDef: DefDef,
      onEnterExpr: Expr[VisitorStack[T] => VisitorControlCommand],
      onEnterDef: DefDef,
      onLeaveExpr: Expr[VisitorStack[T] => VisitorControlCommand],
      onLeaveDef: DefDef,
      members: Seq[KnownMember])

  sealed trait MemberField
  // Used for applying default arguments to a case class
  // ".copy" method, necessary for building Scala 3 AST
  private case class DefaultField(
      idx: Int,
      member: Symbol
  ) extends MemberField

  // Used for actual visiting logic
  private case class KnownMember(
      tpe: TypeRepr,
      member: Symbol,
      memberType: MemberType,
      elemType: TypeRepr
  ) extends MemberField {
    def fullType = (elemType.asType, memberType) match {
      case ('[t], MemberType.Seq) => Type.of[Seq[t]]
      case ('[t], MemberType.List) => Type.of[List[t]]
      case ('[t], MemberType.Vector) => Type.of[Vector[t]]
      case ('[t], MemberType.Option) => Type.of[Option[t]]
      case ('[t], _) => Type.of[t]
    }
  }

  enum MemberType {
    case Seq, List, Vector, Option, Normal, Special
  }

  sealed trait MacroTransformer {
    def matchType: TypeRepr
  }

  case class MacroVisit[T](
      val matchType: TypeRepr,
      val trueType: Type[T],
      val enter: Expr[T => VisitorCommand],
      val leave: Expr[T => VisitorCommand])
      extends MacroTransformer

  case class MacroVisitAnyField[T, S](
      val matchType: TypeRepr,
      val trueMatchType: Type[T],
      val specialType: TypeRepr,
      val trueSpecialType: Type[S],
      val fn: Expr[(T, S) => VisitorCommand],
      val fieldName: Option[String])
      extends MacroTransformer

  private def unsafeAssign[T](using Quotes)(a: Expr[T], b: Expr[T]): Expr[Unit] =
    Assign(a.asTerm, b.asTerm).asExprOf[Unit]

  private def unsafeSelectByName[T, S](using quotes: Quotes)(using
      Type[S])(memberExpr: Expr[T], name: String): Expr[S] = {
    import quotes.reflect._
    Select.unique(memberExpr.asTerm, name).asExprOf[S]
  }
}
