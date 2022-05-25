package sangria.visitor

import scala.quoted._
import scala.compiletime._
import scala.deriving._
import scala.annotation.meta.field

import scala.collection.mutable.{Map => MutableMap}

import scala.annotation.experimental

// Better PR:
// T, M confusion
// Assign ???
object VisitMacro {
  @experimental
  def visitCode[T](rootNode: Expr[T], transformations: Expr[Seq[Transformer[_ <: T]]])(using
    tt: Type[T])(using Quotes): Expr[T] =
      new VisitMacro().visitCode[T](rootNode, transformations)
}

class VisitMacro (using val globalQuotes: Quotes) {
  import globalQuotes.reflect._

  private def collectKnownSubtypes[T](using
      tpe: Type[T]): Set[quotes.reflect.Symbol] = { // TODO check case class, trait etc
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

  
  @experimental
  def visitCode[T](rootNode: Expr[T], transformations: Expr[Seq[Transformer[_ <: T]]])(using
      tt: Type[T]): Expr[T] = {
    import quotes.reflect._

    val subclasses = collectKnownSubtypes[T]

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
        Right(MacroVisitAnyField[u, s](TypeRepr.of[u], Type.of[u], TypeRepr.of[s], Type.of[s], fn, None))
      case '{
            type u <: T
            type s
            VisitAnyFieldByName[`u`, `s`]($fieldName: String, $fn: (`u`, `s`) => VisitorCommand)
          } =>
        val strFieldName = fieldName.valueOrAbort // TODO Reconsider !!!
        Right(MacroVisitAnyField[u, s](TypeRepr.of[u], Type.of[u], TypeRepr.of[s], Type.of[s], fn, Some(strFieldName)))
      case _ => Left("does not know")
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

  @experimental
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
        val (knownMembers: Seq[KnownMember], defaultFields: Seq[DefaultField]) = fieldMembers.partition{
          case _: KnownMember => true
          case _: MemberField => false
        }

        clsTypeRepr.asType match {
          case '[t] => 
            VisitInfo[T, t](
              summon[Type[t]],
              generateOnApplyEdits[T, t](clsTypeRepr, knownMembers, defaultFields),
              generateOnEnter[T](clsTypeRepr, transformationValues, knownMembers),
              generateOnLeave[T](clsTypeRepr, transformationValues),
              knownMembers
            )
        }
      }

    case class IsInstanceOfIf[M](ifCondType: Type[M], ifThen: Expr[VisitorStack[T]])
    
    def genIfs(using q: Quotes)(rootNode: Expr[T], stack: Expr[_root_.sangria.visitor.VisitorStack[T]], breakMode: Expr[Boolean], nestedUpdated: Expr[Option[T]], nestedDeleted: Expr[Boolean]) =
      infos.toList.reverse.map { info => 
        import q.reflect._

        val membersNoSpecial = info.members.filter(_.memberType != MemberType.Special)

        def getMemberSize[M](using q1: Quotes)(using Type[M])(m: KnownMember, currNode: Expr[M]) = {
          import q1.reflect._

          m.memberType match {
            case MemberType.Seq | MemberType.List | MemberType.Vector => Select.unique(Select.unique(currNode.asTerm, m.member.name), "size").asExprOf[Int] // currNode.${m.member.name}.size}
            case MemberType.Option => '{ if( ${ unsafeSelectByName[M, Option[_]](currNode, name(m.member)) }.isEmpty) 0 else 1 }
            case _ => '{1}
          }
        }

        def nextMember(using q1: Quotes)(m: KnownMember): Expr[String] = {
          import q1.reflect._
          val next = membersNoSpecial.dropWhile(_.member.name != m.member.name).drop(1).headOption

          next match {
            case Some(nm) => Expr(name(nm.member))
            case None => '{null}
          }
        }

        def nextMemberSize[M](using Type[M])(m: KnownMember, currNode: Expr[M]) = {
          val next = membersNoSpecial.dropWhile(_.member.name != m.member.name).drop(1).headOption

          next match {
            case Some(nm) => getMemberSize(nm, currNode)
            case None => '{-1}
          }
        }

        def memberNode[M](using q1: Quotes)(using Type[M])(m: KnownMember, currNode: Expr[M], memberIndex: Expr[Int]) = {
          import q1.reflect._
          m.memberType match {
            case MemberType.Seq | MemberType.List | MemberType.Vector =>
              Apply(Select.unique(Select.unique(currNode.asTerm, m.member.name), "apply"), List(memberIndex.asTerm)).asExprOf[T]
            case MemberType.Option =>
              Select.unique(Select.unique(currNode.asTerm, m.member.name), "get").asExprOf[T]
            case MemberType.Normal =>
              Select.unique(currNode.asTerm, m.member.name).asExprOf[T]
            case MemberType.Special => ??? // TODO runtime?
            case _ => ???
          }
        }
        
        def selectNextMemberCase[M](using q1: Quotes)(using Type[M])(isLeavingOut: Expr[Boolean], memberNodOut: Expr[Option[T]], currMemberOut: Expr[String], memberIndexOut: Expr[Int], memberSizeOut: Expr[Int], currNode: Expr[M]) =
          membersNoSpecial.zipWithIndex.map { case (m, idx) =>
            import q1.reflect._

            val rhs = 
              '{
                var isLeaving = $isLeavingOut
                var memberNod = $memberNodOut
                var currMember = $currMemberOut
                var memberIndex = $memberIndexOut
                var memberSize = $memberSizeOut

                if (memberIndex <= (memberSize - 1)) {
                  memberNod = Option(${memberNode[M](m, currNode, '{memberIndex})}.asInstanceOf[T])
                } else {
                  memberNod = None
                  currMember = ${nextMember(m)}
                  memberIndex = -1
                  
                  memberSize = ${nextMemberSize[M](m, currNode)}

                  if (currMember == null) {
                    isLeaving = true
                  }
                }
                (isLeaving, memberNod, currMember, memberIndex, memberSize)
              }
            CaseDef(Expr(name(m.member)).asTerm, None, rhs.asTerm)
          }

        def selectNextMember[M](using q1: Quotes)(using Type[M])(isLeaving: Expr[Boolean], memberNod: Expr[Option[T]], currMember: Expr[String], memberIndex: Expr[Int], memberSize: Expr[Int], currNode: Expr[M]): Expr[(Boolean, Option[T], String, Int, Int)] = {
          import q1.reflect._
          Match(currMember.asTerm, selectNextMemberCase(isLeaving, memberNod, currMember, memberIndex, memberSize, currNode).toList).asExprOf[(Boolean, Option[T], String, Int, Int)]
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
                    $stack.edits = _root_.scala.collection.mutable.Map()

                  $stack.edits.get(currMember) match {
                    case Some(me) => me
                    case None =>
                      $stack.edits(currMember) = _root_.scala.collection.mutable.Map[Int, Option[T]]()
                  }

                  if ($nestedDeleted) {
                    $stack.edits(currMember)(memberIndex) = null
                  } else if ($nestedUpdated != None) {
                    $stack.edits(currMember)(memberIndex) = $nestedUpdated
                  }

                  ${ unsafeAssign(nestedDeleted, '{false}) }
                  ${ unsafeAssign(nestedUpdated, '{None}) }
                }
                if (!$breakMode) {
                  var enterResult: _root_.sangria.visitor.VisitorCommand = _root_.sangria.visitor.VisitorCommand.Continue

                  var currNode = $stack.node.get.asInstanceOf[t]

                  if (currMember == null) {
                    enterResult = ${info.onEnterExpr}($stack)
                    currMember = ${Expr(name(membersNoSpecial.head.member))}
                    memberSize = ${getMemberSize[t](membersNoSpecial.head, 'currNode)}
                  }

                  enterResult match {
                    case _root_.sangria.visitor.VisitorCommand.Continue =>
                      while({
                        memberIndex = memberIndex + 1
                        val out = ${selectNextMember[t]('isLeaving, 'memberNod, 'currMember, 'memberIndex, 'memberSize, 'currNode)}
                        isLeaving = out._1
                        memberNod = out._2
                        currMember = out._3
                        memberIndex = out._4
                        memberSize = out._5 // TODO this is dumb
                        memberNod == None && !isLeaving
                      }) ()

                    case _root_.sangria.visitor.VisitorCommand.Skip =>
                      isLeaving = true

                    case _root_.sangria.visitor.VisitorCommand.Break =>
                      ${ unsafeAssign(breakMode, '{true}) }
                      
                      isLeaving = true
                  }
                } else {
                  isLeaving = true
                }

                $stack.currMember = currMember
                $stack.memberIndex = memberIndex
                $stack.memberSize = memberSize
                if (isLeaving) {
                  if (($stack.edits != null && $stack.edits.nonEmpty) || ($stack.specialEdits != null && $stack.specialEdits.nonEmpty))
                    $stack.node = ${info.applyEditsExpr}($stack).asInstanceOf[Option[T]]
                  val leaveResult = ${info.onLeaveExpr}($stack)

                  if (leaveResult == _root_.sangria.visitor.VisitorCommand.Break)
                    ${ unsafeAssign(breakMode, '{true}) }

                  ${ unsafeAssign(nestedUpdated, '{if ($stack.updated) $stack.node else null}) } // TODO NONE?

                  ${ unsafeAssign(nestedDeleted, '{$stack.deleted}) }
                  
                  $stack.prev
                } else {
                  var next = $stack.next

                  if (next != null) {
                    next.node = memberNod.asInstanceOf[Option[T]] // TODO added
                    next.updated = false
                    next.deleted = false
                    next.currMember = null
                    next.memberIndex = -1
                    next.memberSize = -1
                    next.edits = null
                    next.specialEdits = null
                  } else {
                    next = new _root_.sangria.visitor.VisitorStack[T](memberNod.asInstanceOf[Option[T]], false, false, null, -1, -1, null, null, $stack, null)
                  }
                  // stack = next // assign
                  next
                }
              }
            } else {
              '{
                val enterResult = ${info.onEnterExpr}($stack)

                if (enterResult == _root_.sangria.visitor.VisitorCommand.Break)
                  ${ Assign(breakMode.asTerm, '{true}.asTerm).asExprOf[Unit] }

                if (${stack}.specialEdits != null && ${stack}.specialEdits.nonEmpty)
                  ${stack}.node = ${info.applyEditsExpr}($stack).asInstanceOf[Option[T]]

                val leaveResult = ${info.onLeaveExpr}($stack)

                if (leaveResult == _root_.sangria.visitor.VisitorCommand.Break)
                  ${ Assign(breakMode.asTerm, '{true}.asTerm).asExprOf[Unit] }

                ${ Assign(nestedUpdated.asTerm, '{if (${stack}.updated) ${stack}.node else None}.asTerm).asExprOf[Unit] }

                ${ Assign(nestedDeleted.asTerm, '{${stack}.deleted}.asTerm).asExprOf[Unit] }

                // stack = stack.prev
                $stack.prev
              }
            }
          }
          
          IsInstanceOfIf[t](Type.of[t], logic)
          // val bind = Symbol.newBind(Symbol.spliceOwner, "c", Flags.EmptyFlags, TypeTree.of[Option[t]])
          // TOD CaseDef(Bind(bind, Typed(Ref(bind)), TypeTree.of[Option[t]]), None, logic.asTerm)
        }
      }

    def genMatch(using q: Quotes)(rootNode: Expr[T], stack: Expr[VisitorStack[T]], breakMode: Expr[Boolean], nestedUpdated: Expr[Option[T]], nestedDeleted: Expr[Boolean]): Expr[VisitorStack[T]] = {
      import q.reflect._
      val isInstanceOfIfs = genIfs(rootNode, stack, breakMode, nestedUpdated, nestedDeleted)
      val elseThrow = '{ throw new IllegalStateException("Match") }.asTerm
      isInstanceOfIfs.foldRight(elseThrow) { case (IsInstanceOfIf(ifType, ifThen), elseIf) =>
        ifType match {
          case '[t] => If(TypeApply(Select.unique('{$stack.node.getOrElse(null)}.asTerm, "isInstanceOf"), List(TypeTree.of[t])), ifThen.asTerm, elseIf)
        }
      }.asExprOf[VisitorStack[T]]
      // Match('{$stack.node}.asTerm, genCases(rootNode, stack, breakMode, nestedUpdated, nestedDeleted).toList).asExprOf[VisitorStack[T]]
    }

    '{
      val rootNode = $node
      var stack = _root_.sangria.visitor.VisitorStack.initial[tpe.Underlying](rootNode)
      var breakMode = false
      var nestedUpdated: Option[tpe.Underlying] = None
      var nestedDeleted = false

      while({
        stack = ${ genMatch('rootNode, 'stack, 'breakMode, 'nestedUpdated, 'nestedDeleted) }
        stack != null
      }) ()
      nestedUpdated.getOrElse(rootNode)
    }
  }

  private def generateOnEnter[T](using tpe: Type[T])(typeRepr: TypeRepr, tx: Seq[MacroTransformer], members: Seq[KnownMember]): Expr[_root_.sangria.visitor.VisitorStack[T] => _root_.sangria.visitor.VisitorCommand] = {
    def enterLogic(t: MacroTransformer): Expr[((sangria.visitor.VisitorCommand, sangria.visitor.VisitorStack[T])) => (sangria.visitor.VisitorCommand, sangria.visitor.VisitorStack[T])] =
      t match {
        case visit: MacroVisit[mt] =>
          given Type[mt] = visit.trueType
          '{
            (enterResult: _root_.sangria.visitor.VisitorCommand, stack: _root_.sangria.visitor.VisitorStack[T]) =>
              if (enterResult == _root_.sangria.visitor.VisitorCommand.Continue) {
                (${visit.enter}(stack.node.getOrElse(null).asInstanceOf[mt]): _root_.sangria.visitor.VisitorCommand) match {
                  case cc: _root_.sangria.visitor.VisitorControlCommand =>
                    (cc, stack)
                  case tr: _root_.sangria.visitor.VisitorCommand.Transform[_] =>
                    stack.updated = true
                    stack.node = Option(tr.newValue.asInstanceOf[T])
                    (tr.controlCommand, stack)
                  case _root_.sangria.visitor.VisitorCommand.Delete =>
                    stack.deleted = true
                    (_root_.sangria.visitor.VisitorCommand.Skip, stack)
                  case _root_.sangria.visitor.VisitorCommand.DeleteAndBreak =>
                    stack.deleted = true
                    (_root_.sangria.visitor.VisitorCommand.Break, stack)
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
                special.fieldName.fold(true)(fn => name(m.member) == fn))

          val specialCode: Seq[Expr[((sangria.visitor.VisitorCommand, sangria.visitor.VisitorStack[T])) => (sangria.visitor.VisitorCommand, sangria.visitor.VisitorStack[T])]] =
            specialMembers.map { sm =>
              '{
                (enterResult: _root_.sangria.visitor.VisitorCommand, stack: _root_.sangria.visitor.VisitorStack[T]) =>
                  if (enterResult == _root_.sangria.visitor.VisitorCommand.Continue) {
                    val actualNode = stack.node.getOrElse(null).asInstanceOf[mt]

                    (${special.fn}(actualNode, ${Select.unique('{actualNode}.asTerm, sm.member.name).asExprOf[st]}): _root_.sangria.visitor.VisitorCommand) match {
                      case cc: _root_.sangria.visitor.VisitorControlCommand =>
                        (cc, stack)
                      case tr: _root_.sangria.visitor.VisitorCommand.Transform[_] =>
                        if (stack.specialEdits == null)
                          stack.specialEdits = _root_.scala.collection.mutable.Map()

                        stack.specialEdits(${Expr(name(sm.member))}) = tr.newValue
                        (tr.controlCommand, stack)
                      case _root_.sangria.visitor.VisitorCommand.Delete =>
                        if (stack.specialEdits == null)
                          stack.specialEdits = _root_.scala.collection.mutable.Map()

                        stack.specialEdits(${Expr(name(sm.member))}) = null
                        (_root_.sangria.visitor.VisitorCommand.Skip, stack)
                      case _root_.sangria.visitor.VisitorCommand.DeleteAndBreak =>
                        if (stack.specialEdits == null)
                          stack.specialEdits = _root_.scala.collection.mutable.Map()

                        stack.specialEdits(${Expr(name(sm.member))}) = null
                        (_root_.sangria.visitor.VisitorCommand.Break, stack)
                    }
                  } else {
                    (enterResult, stack)
                  }
              }
            }

          '{
            (enterResult: _root_.sangria.visitor.VisitorCommand, stack: _root_.sangria.visitor.VisitorStack[T]) =>
              ${flattenExprs[(sangria.visitor.VisitorCommand, sangria.visitor.VisitorStack[T])](specialCode)} (enterResult, stack)
          }

    }
    '{
      (stack: VisitorStack[T]) => {
        val enterResult: _root_.sangria.visitor.VisitorControlCommand = _root_.sangria.visitor.VisitorCommand.Continue

        // enterResult
        ${flattenExprs[(sangria.visitor.VisitorCommand, sangria.visitor.VisitorStack[T])](transformersForType(typeRepr, tx).map(enterLogic))}(enterResult, stack)._1 // returns enterResult
      }
    }
  }

  private def generateOnLeave[T](using Type[T])(using quotes: Quotes)(typeRepr: TypeRepr, tx: Seq[MacroTransformer]) = {
      def leaveLogic(t: MacroTransformer): Expr[((sangria.visitor.VisitorCommand, sangria.visitor.VisitorStack[T])) => (sangria.visitor.VisitorCommand, sangria.visitor.VisitorStack[T])] =
        t match {
          case visit: MacroVisit[mt] =>
            given Type[mt] = visit.trueType
            '{
              (leaveResult: _root_.sangria.visitor.VisitorCommand, stack: _root_.sangria.visitor.VisitorStack[T]) =>
                if (leaveResult == _root_.sangria.visitor.VisitorCommand.Continue) {
                  (${visit.leave}(stack.node.getOrElse(null).asInstanceOf[mt]): _root_.sangria.visitor.VisitorCommand) match {
                    case cc: _root_.sangria.visitor.VisitorControlCommand =>
                      (cc, stack)
                    case tr: _root_.sangria.visitor.VisitorCommand.Transform[_] =>
                      stack.updated = true
                      stack.node = Option(tr.newValue.asInstanceOf[T])
                      (tr.controlCommand, stack)
                    case _root_.sangria.visitor.VisitorCommand.Delete =>
                      stack.deleted = true
                      (_root_.sangria.visitor.VisitorCommand.Skip, stack)
                    case _root_.sangria.visitor.VisitorCommand.DeleteAndBreak =>
                      stack.deleted = true
                      (_root_.sangria.visitor.VisitorCommand.Break, stack)
                  }
                } else (leaveResult, stack)
            }

          case special: MacroVisitAnyField[mt, ms] => '{ (leaveResult, stack) => (leaveResult, stack) }
        }
      
      '{
        (stack: VisitorStack[T]) => {
          var leaveResult: _root_.sangria.visitor.VisitorControlCommand = _root_.sangria.visitor.VisitorCommand.Continue

          ${flattenExprs[(sangria.visitor.VisitorCommand, sangria.visitor.VisitorStack[T])](transformersForType(typeRepr, tx).map(leaveLogic))}(leaveResult, stack)._1
        }
      }
    }
  
  private def generateOnApplyEdits[T, M](using Type[T], Type[M])(infoTypeRepr: TypeRepr, members: Seq[KnownMember], defaultFields: Seq[DefaultField]): Expr[VisitorStack[T] => Option[T]] = {
    def applyActualMemberEdits(using q: Quotes)(m: KnownMember, edits: Expr[MutableMap[Int, Option[T]]], origNode: Expr[M]) =
      (m.elemType.asType, m.fullType) match
        case ('[t], '[f]) =>
          m.memberType match { // stack T
            case MemberType.Normal =>
              '{
                if ($edits(0) == null) then
                  ${Select.unique(origNode.asTerm, m.member.name).asExprOf[t]}
                else
                  $edits(0).orNull.asInstanceOf[t]
              }
            case MemberType.Option =>
              '{
                // $edits(0).getOrElse(null).asInstanceOf[t]
                if ($edits(0) == null || $edits(0).getOrElse(null) == null) // TODO beautify
                  _root_.scala.None
                else
                  _root_.scala.Some($edits(0).orNull.asInstanceOf[t])
              }

            case MemberType.List | MemberType.Vector | MemberType.Seq =>
              '{
                val orig = ${ 
                  m.memberType match 
                    case MemberType.List => Select.unique(origNode.asTerm, m.member.name).asExprOf[List[t]]
                    case MemberType.Vector => Select.unique(origNode.asTerm, m.member.name).asExprOf[Vector[t]]
                    case MemberType.Seq => Select.unique(origNode.asTerm, m.member.name).asExprOf[Seq[t]]
                }
                val builder = new _root_.scala.collection.immutable.VectorBuilder[t]
                var idx = 0

                while (idx < orig.size) {
                  $edits.get(idx) match {
                    case Some(None) | Some(null) => // do nothing - element is deleted
                    case Some(Some(elem)) => builder += elem.asInstanceOf[t]
                    case None => builder += orig(idx)
                  }

                  idx += 1
                }

                ${m.memberType match {
                  case MemberType.List => '{builder.result().toList}
                  case _ => '{builder.result()}
                }}
              }

            case MemberType.Special => ???

    }

    def applyMemberEdits(using q: Quotes)(m: KnownMember, stack: Expr[VisitorStack[T]], origNode: Expr[M]) = // generates unknown type
      if (m.memberType != MemberType.Special) {
        import q.reflect._
        
        m.fullType match
          case '[t] =>
            '{
              if (${stack}.edits != null)
                ${stack}.edits.get(${Expr(name(m.member))}) match {
                  case Some(edits) if edits != null && edits.nonEmpty =>
                    ${ applyActualMemberEdits(m, 'edits, origNode) }

                  case None =>
                    ${ Select.unique(origNode.asTerm, m.member.name).asExprOf[t] }
                }
              else ${ Select.unique(origNode.asTerm, m.member.name).asExprOf[t] } // TODO consider storing type
            }
      } else
        m.fullType match
          case '[t] =>
            '{
              if ($stack.specialEdits != null)
                $stack.specialEdits.get(${Expr(name(m.member))}) match {
                  case Some(edit) if edit != null =>
                    edit.asInstanceOf[t]

                  case _ =>
                    ${ Select.unique(origNode.asTerm, m.member.name).asExprOf[t] }
                }
              else ${ Select.unique(origNode.asTerm, m.member.name).asExprOf[t] }
            }

    def generateModifiedOrigNode(using q: Quotes)(origNode: Expr[M], stack: Expr[VisitorStack[T]]): Expr[M] = {
      import q.reflect._
      val copyList = members.map(m => NamedArg(m.member.name, applyMemberEdits(m, stack, origNode).asTerm)).toList
      val defaultList = defaultFields.map(f => NamedArg(f.member.name, Select(origNode.asTerm, origNode.asTerm.tpe.typeSymbol.methodMember("copy$default$" + (f.idx + 1).toString).head)))

      if (!copyList.isEmpty)
        Apply(Select(origNode.asTerm, origNode.asTerm.tpe.typeSymbol.methodMember("copy").head), copyList ++ defaultList).asExprOf[M]
      else
        origNode
    }

    '{
      (stack: VisitorStack[T]) => {
        stack.updated = true

        val origNodeMaybe = stack.node.asInstanceOf[Option[M]]
        
        origNodeMaybe.map(origNode => ${generateModifiedOrigNode('origNode, 'stack)}.asInstanceOf[T])
      }
    }
  }
  
  def flattenExprs[T](using Type[T], Quotes)(exprs: Seq[Expr[T => T]]): Expr[T => T] =
    if (exprs.isEmpty) '{(a: T) => a}
    else '{ (t: T) => ${exprs.head}(${flattenExprs(exprs.tail)}(t)) }
  
  def transformersForType(tpe: TypeRepr, tx: Seq[MacroTransformer]) =
    tx.filter(t => isSupertype1(t.matchType, tpe))
  
  private def isSupertype1(t: TypeRepr, subtype: TypeRepr) =
    subtype <:< t  // TODO With erasures?

  @experimental
  private def findMemberType(baseType: TypeRepr, name: String,
    fieldType: TypeRepr, specials: Seq[MacroVisitAnyField[?, ?]]): Option[(TypeRepr, MemberType)] = {

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
      case '[t] if specials.exists(s => fieldType <:< s.specialType && s.fieldName.fold(true)(fn => name == fn)) =>
        Some(fieldType -> MemberType.Special)
      case '[t] => None
    }
  }
  @experimental
  private def findKnownMembers[T](using baseTpe: Type[T])(
    symbol: Symbol,
    specials: Seq[MacroVisitAnyField[?, ?]]
  ): Seq[MemberField] = {
    val baseTypeTree = TypeTree.of[T]
    val baseTypeRepr = baseTypeTree.tpe
    val baseSymbol: Symbol = baseTypeTree.symbol
    symbol.caseFields.zipWithIndex.map { (m, idx) =>
      m.tree match {
        case ValDef(_, typeTree, _) =>
            val caseFieldType: TypeRepr = typeTree.tpe
            val memberType = findMemberType(baseTypeRepr, name(m), caseFieldType, specials)
            memberType.map { case (et, mt) =>
              KnownMember(baseTypeRepr, m, mt, et)
            }.getOrElse(DefaultField(idx, m))
        case other => throw(new Exception("ERROR: unhandled tree: " + other))
      }
    }
    .toList
    .reverse
  }
  
  // TODO remove (?)
  private def name(symbol: Symbol): String = // TODO decoded name ???
    symbol.name

  private case class VisitInfo[T, M](
    tpe: Type[M],
    applyEditsExpr: Expr[_root_.sangria.visitor.VisitorStack[T] => Option[T]],
    onEnterExpr: Expr[_root_.sangria.visitor.VisitorStack[T] => _root_.sangria.visitor.VisitorCommand],
    onLeaveExpr: Expr[_root_.sangria.visitor.VisitorStack[T] => _root_.sangria.visitor.VisitorCommand],
    members: Seq[KnownMember])

  sealed trait MemberField
  // used for applying default arguments to a case class
  // ".copy" method, necessary for building Scala 3 AST
  private case class DefaultField(
    idx: Int, member: Symbol
  ) extends MemberField
  // used for visiting
  private case class KnownMember(
    tpe: TypeRepr,
    member: Symbol,
    memberType: MemberType,
    elemType: TypeRepr,
    ) extends MemberField {
      def fullType = (elemType.asType, memberType) match {
        case ('[t], MemberType.Seq) => Type.of[Seq[t]]
        case ('[t], MemberType.List) => Type.of[List[t]]
        case ('[t], MemberType.Vector) => Type.of[Vector[t]]
        case ('[t], MemberType.Option) => Type.of[Option[t]]
        case ('[t], other) => Type.of[t]
      }
    }

  enum MemberType {
    case Seq, List, Vector, Option, Normal, Special
  }

  private def unsafeAssign[T](using Quotes)(a: Expr[T], b: Expr[T]): Expr[Unit] =
    Assign(a.asTerm, b.asTerm).asExprOf[Unit]

  private def unsafeSelectByName[T, S](using Quotes, Type[S])(memberExpr: Expr[T], name: String): Expr[S] =
    Select.unique(memberExpr.asTerm, name).asExprOf[S]
}
