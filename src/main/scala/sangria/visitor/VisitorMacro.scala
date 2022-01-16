package sangria.visitor

import scala.reflect.macros.blackbox

class VisitMacro(val c: blackbox.Context) {
  import c.universe._

  def visitImpl[T: WeakTypeTag](rootNode: Tree, transformations: Tree*) = {
    val validatedConfig = validateTransformations(transformations)
    val errors = validatedConfig.collect { case Left(error) => error }

    if (errors.nonEmpty) reportErrors(errors)
    else {
      val validConfig = validatedConfig.collect { case Right(cfg) => cfg }
      val tpe = weakTypeTag[T].tpe
      val subclasses = collectKnownSubtypes(tpe.typeSymbol)

      generateTraversal(tpe, rootNode, subclasses, validConfig)
    }
  }

  def generateTraversal(
      tpe: Type,
      node: Tree,
      subclasses: Set[Symbol],
      tx: Seq[MacroTransformer]) = {
    val specials = tx.collect { case s: MacroVisitAnyField => s }

    val infos =
      subclasses.toVector.map { cls =>
        VisitInfo(
          cls,
          c.freshName("applyEdits_" + name(cls)),
          c.freshName("onEnter_" + name(cls)),
          c.freshName("onLeave_" + name(cls)),
          findKnownMembers(tpe, cls.typeSignature, specials)
        )
      }

    val applyEdits = generateApplyEdits(tpe, infos)
    val onEnter = generateOnEnter(tpe, infos, tx)
    val onLeave = generateOnLeave(tpe, infos, tx)

    val cases =
      infos.map { info =>
        val cls = info.tpe
        val membersNoSpecial = info.members.filter(_.memberType != MemberType.Special)

        def memberSize(m: KnownMember) =
          if (MemberType.coll(m.memberType))
            q"currNode.${m.member.name}.size"
          else
            q"1"

        def nextMember(m: KnownMember) = {
          val next = membersNoSpecial.dropWhile(_.member.name != m.member.name).drop(1).headOption

          next match {
            case Some(nm) => q"${name(nm.member)}"
            case None => q"null"
          }
        }

        def nextMemberSize(m: KnownMember) = {
          val next = membersNoSpecial.dropWhile(_.member.name != m.member.name).drop(1).headOption

          next match {
            case Some(nm) => memberSize(nm)
            case None => q"-1"
          }
        }

        def memberNode(m: KnownMember) =
          m.memberType match {
            case MemberType.Seq | MemberType.List | MemberType.Vector =>
              q"currNode.${m.member.name}(memberIndex)"
            case MemberType.Option =>
              q"currNode.${m.member.name}.get"
            case MemberType.Normal =>
              q"currNode.${m.member.name}"
            case MemberType.Special =>
              q"???" // this should not be used
          }

        val selectNextMemberCase =
          membersNoSpecial.zipWithIndex.map { case (m, idx) =>
            cq"""
              ${name(m.member)} =>
                if (memberIndex <= (memberSize - 1)) {
                  memberNode = ${memberNode(m)}
                } else {
                  memberNode = null
                  currMember = ${nextMember(m)}
                  memberIndex = -1
                  memberSize = ${nextMemberSize(m)}

                  if (currMember == null) {
                    isLeaving = true
                  }
                }
            """
          }

        val selectNextMember =
          q"""
            currMember match {
              case ..$selectNextMemberCase
            }
          """

        val logic =
          if (membersNoSpecial.nonEmpty)
            q"""
              var isLeaving = false
              var memberNode: $tpe = null
              var currMember = stack.currMember
              var memberIndex = stack.memberIndex
              var memberSize = stack.memberSize

              if (nestedUpdated != null || nestedDeleted) {
                if (stack.edits == null)
                  stack.edits = _root_.scala.collection.mutable.Map()

                stack.edits.get(currMember) match {
                  case Some(me) => me
                  case None =>
                    stack.edits(currMember) = _root_.scala.collection.mutable.Map[Int, $tpe]()
                }

                if (nestedDeleted) {
                  stack.edits(currMember)(memberIndex) = null
                } else if (nestedUpdated != null) {
                  stack.edits(currMember)(memberIndex) = nestedUpdated
                }

                nestedDeleted = false
                nestedUpdated = null
              }

              if (!breakMode) {
                var enterResult: _root_.sangria.visitor.VisitorControlCommand = _root_.sangria.visitor.VisitorCommand.Continue

                val currNode = stack.node.asInstanceOf[$cls]

                if (currMember == null) {
                  enterResult = ${TermName(info.onEnterName)}(stack)
                  currMember = ${name(membersNoSpecial.head.member)}
                  memberSize = ${memberSize(membersNoSpecial.head)}
                }

                enterResult match {
                  case _root_.sangria.visitor.VisitorCommand.Continue =>
                    do {
                      memberIndex = memberIndex + 1
                      $selectNextMember
                    } while (memberNode == null && !isLeaving)

                  case _root_.sangria.visitor.VisitorCommand.Skip =>
                    isLeaving = true

                  case _root_.sangria.visitor.VisitorCommand.Break =>
                    breakMode = true
                    isLeaving = true
                }
              } else {
                isLeaving = true
              }

              stack.currMember = currMember
              stack.memberIndex = memberIndex
              stack.memberSize = memberSize

              if (isLeaving) {
                if ((stack.edits != null && stack.edits.nonEmpty) || (stack.specialEdits != null && stack.specialEdits.nonEmpty))
                  stack.node = ${TermName(info.applyEditsName)}(stack)

                val leaveResult = ${TermName(info.onLeaveName)}(stack)

                if (leaveResult == _root_.sangria.visitor.VisitorCommand.Break)
                  breakMode = true

                nestedUpdated = if (stack.updated) stack.node else null
                nestedDeleted = stack.deleted

                stack = stack.prev
              } else {
                var next = stack.next

                if (next != null) {
                  next.node = memberNode
                  next.updated = false
                  next.deleted = false
                  next.currMember = null
                  next.memberIndex = -1
                  next.memberSize = -1
                  next.edits = null
                  next.specialEdits = null
                } else {
                  next = new _root_.sangria.visitor.VisitorStack[$tpe](memberNode, false, false, null, -1, -1, null, null, stack, null)
                }

                stack = next
              }
            """
          else
            q"""
              val enterResult = ${TermName(info.onEnterName)}(stack)

              if (enterResult == _root_.sangria.visitor.VisitorCommand.Break)
                breakMode = true

             if (stack.specialEdits != null && stack.specialEdits.nonEmpty)
                stack.node = ${TermName(info.applyEditsName)}(stack)

              val leaveResult = ${TermName(info.onLeaveName)}(stack)

              if (leaveResult == _root_.sangria.visitor.VisitorCommand.Break)
                breakMode = true

              nestedUpdated = if (stack.updated) stack.node else null

              nestedDeleted = stack.deleted

              stack = stack.prev
            """

        cq"n: $cls => $logic"
      }

    q"""
      ..$applyEdits
      ..$onEnter
      ..$onLeave

      val rootNode = $node
      var stack = new _root_.sangria.visitor.VisitorStack[$tpe](rootNode, false, false, null, -1, -1, null, null, null, null)
      var breakMode = false
      var nestedUpdated: $tpe = null
      var nestedDeleted = false

      do {
        stack.node match {
          case ..$cases
        }
      } while(stack != null)

      if (nestedUpdated != null) nestedUpdated else rootNode
    """
  }

  private def generateOnEnter(tpe: Type, infos: Seq[VisitInfo], tx: Seq[MacroTransformer]) =
    infos.map { info =>
      def enterLogic(t: MacroTransformer) = t match {
        case visit: MacroVisit =>
          q"""
            if (enterResult == _root_.sangria.visitor.VisitorCommand.Continue) {
              (${visit.enter}(stack.node.asInstanceOf[${t.matchType}]): _root_.sangria.visitor.VisitorCommand) match {
                case cc: _root_.sangria.visitor.VisitorControlCommand =>
                  enterResult = cc
                case tr: _root_.sangria.visitor.VisitorCommand.Transform[_] =>
                  stack.updated = true
                  stack.node = tr.newValue.asInstanceOf[$tpe]
                  enterResult = tr.controlCommand
                case _root_.sangria.visitor.VisitorCommand.Delete =>
                  stack.deleted = true
                  enterResult = _root_.sangria.visitor.VisitorCommand.Skip
                case _root_.sangria.visitor.VisitorCommand.DeleteAndBreak =>
                  stack.deleted = true
                  enterResult = _root_.sangria.visitor.VisitorCommand.Break
              }
            }
          """

        case special: MacroVisitAnyField =>
          val specialMembers =
            info.members.filter(m =>
              m.memberType == MemberType.Special &&
                isSupertypeNoErasure(special.specialType, m.elemType) &&
                special.fieldName.fold(true)(fn => name(m.member) == fn))

          val specialCode =
            specialMembers.map { sm =>
              q"""
                if (enterResult == _root_.sangria.visitor.VisitorCommand.Continue) {
                  val actualNode = stack.node.asInstanceOf[${t.matchType}]

                  (${special.fn}(actualNode, actualNode.${sm.member.name}): _root_.sangria.visitor.VisitorCommand) match {
                    case cc: _root_.sangria.visitor.VisitorControlCommand =>
                      enterResult = cc
                    case tr: _root_.sangria.visitor.VisitorCommand.Transform[_] =>
                      if (stack.specialEdits == null)
                        stack.specialEdits = _root_.scala.collection.mutable.Map()

                      stack.specialEdits(${name(sm.member)}) = tr.newValue
                      enterResult = tr.controlCommand
                    case _root_.sangria.visitor.VisitorCommand.Delete =>
                      if (stack.specialEdits == null)
                        stack.specialEdits = _root_.scala.collection.mutable.Map()

                      stack.specialEdits(${name(sm.member)}) = null
                      enterResult = _root_.sangria.visitor.VisitorCommand.Skip
                    case _root_.sangria.visitor.VisitorCommand.DeleteAndBreak =>
                      if (stack.specialEdits == null)
                        stack.specialEdits = _root_.scala.collection.mutable.Map()

                      stack.specialEdits(${name(sm.member)}) = null
                      enterResult = _root_.sangria.visitor.VisitorCommand.Break
                  }
                }
              """
            }

          q"..$specialCode"
      }

      q"""
        def ${TermName(
        info.onEnterName)}(stack: VisitorStack[$tpe]): _root_.sangria.visitor.VisitorControlCommand = {
          var enterResult: _root_.sangria.visitor.VisitorControlCommand = _root_.sangria.visitor.VisitorCommand.Continue

          ..${transformersForType(info.tpe.asType.toType, tx).map(enterLogic)}

          enterResult
        }
      """
    }

  private def generateOnLeave(tpe: Type, infos: Seq[VisitInfo], tx: Seq[MacroTransformer]) =
    infos.map { info =>
      def leaveLogic(t: MacroTransformer) = t match {
        case visit: MacroVisit =>
          q"""
              if (leaveResult == _root_.sangria.visitor.VisitorCommand.Continue) {
                (${visit.leave}(stack.node.asInstanceOf[${t.matchType}]): _root_.sangria.visitor.VisitorCommand) match {
                  case cc: _root_.sangria.visitor.VisitorControlCommand =>
                    leaveResult = cc
                  case tr: _root_.sangria.visitor.VisitorCommand.Transform[_] =>
                    stack.updated = true
                    stack.node = tr.newValue.asInstanceOf[$tpe]
                    leaveResult = tr.controlCommand
                  case _root_.sangria.visitor.VisitorCommand.Delete =>
                    stack.deleted = true
                    leaveResult = _root_.sangria.visitor.VisitorCommand.Skip
                  case _root_.sangria.visitor.VisitorCommand.DeleteAndBreak =>
                    stack.deleted = true
                    leaveResult = _root_.sangria.visitor.VisitorCommand.Break
                }
              }
            """

        case special: MacroVisitAnyField => q""
      }

      q"""
        def ${TermName(
        info.onLeaveName)}(stack: VisitorStack[$tpe]): _root_.sangria.visitor.VisitorControlCommand = {
          var leaveResult: _root_.sangria.visitor.VisitorControlCommand = _root_.sangria.visitor.VisitorCommand.Continue

          ..${transformersForType(info.tpe.asType.toType, tx).map(leaveLogic)}

          leaveResult
        }
      """
    }

  private def generateApplyEdits(tpe: Type, infos: Seq[VisitInfo]) =
    infos.map { info =>
      def applyActualMemberEdits(m: KnownMember) = m.memberType match {
        case MemberType.Normal =>
          q"""
            if (edits(0) == null)
              origNode.${m.member.name}
            else
              edits(0).asInstanceOf[${m.elemType}]
          """
        case MemberType.Option =>
          q"""
            if (edits(0) == null)
              _root_.scala.None
            else
              _root_.scala.Some(edits(0).asInstanceOf[${m.elemType}])
          """

        case MemberType.List | MemberType.Vector | MemberType.Seq =>
          q"""
            val orig = origNode.${m.member.name}
            val builder = new _root_.scala.collection.immutable.VectorBuilder[${m.elemType}]
            var idx = 0

            while (idx < orig.size) {
              edits.get(idx) match {
                case Some(null) => // do nothing - element is deleted
                case Some(elem) => builder += elem.asInstanceOf[${m.elemType}]
                case None => builder += orig(idx)
              }

              idx += 1
            }

            ${m.memberType match {
            case MemberType.List => q"builder.result().toList"
            case _ => q"builder.result()"
          }}
          """

        case MemberType.Special =>
          q"???" // should not be used

      }

      def applyMemberEdits(m: KnownMember) =
        if (m.memberType != MemberType.Special)
          q"""
            if (stack.edits != null)
              stack.edits.get(${name(m.member)}) match {
                case Some(edits) if edits != null && edits.nonEmpty =>
                  ${applyActualMemberEdits(m)}

                case None =>
                  origNode.${m.member.name}
              }
            else origNode.${m.member.name}
          """
        else
          q"""
            if (stack.specialEdits != null)
              stack.specialEdits.get(${name(m.member)}) match {
                case Some(edit) if edit != null =>
                  edit.asInstanceOf[${m.elemType}]

                case _ =>
                  origNode.${m.member.name}
              }
            else origNode.${m.member.name}
          """

      q"""
        def ${TermName(info.applyEditsName)}(stack: VisitorStack[$tpe]) = {
          stack.updated = true

          val origNode = stack.node.asInstanceOf[${info.tpe}]

          origNode.copy(..${info.members.map(m => q"${m.member.name} = ${applyMemberEdits(m)}")})
        }
      """
    }

  def transformersForType(tpe: Type, tx: Seq[MacroTransformer]) =
    tx.filter(t => isSupertype1(t.matchType, tpe))

  private def name(s: Symbol) = s.name.decodedName.toString

  private def findKnownMembers(
      baseType: Type,
      tpe: Type,
      specials: Seq[MacroVisitAnyField]): List[KnownMember] =
    tpe.members
      .flatMap {
        case m: MethodSymbol if m.isCaseAccessor =>
          findMemberType(baseType, name(m), m.returnType, specials).map { case (et, mt) =>
            new KnownMember(tpe, m, mt, et)
          }
        case _ => None
      }
      .toList
      .reverse

  private case class VisitInfo(
      tpe: Symbol,
      applyEditsName: String,
      onEnterName: String,
      onLeaveName: String,
      members: Seq[KnownMember])
  private case class KnownMember(
      tpe: Type,
      member: MethodSymbol,
      memberType: MemberType.Value,
      elemType: Type)

  private def findMemberType(
      baseType: Type,
      name: String,
      fieldType: Type,
      specials: Seq[MacroVisitAnyField]): Option[(Type, MemberType.Value)] =
    if (isSupertype[List[_]](fieldType) && fieldType.typeArgs.nonEmpty && isSupertype1(
        baseType,
        fieldType.typeArgs.head))
      Some(fieldType.typeArgs.head -> MemberType.List)
    else if (isSupertype[Vector[_]](fieldType) && fieldType.typeArgs.nonEmpty && isSupertype1(
        baseType,
        fieldType.typeArgs.head))
      Some(fieldType.typeArgs.head -> MemberType.Vector)
    else if (isSupertype[Seq[_]](fieldType) && fieldType.typeArgs.nonEmpty && isSupertype1(
        baseType,
        fieldType.typeArgs.head))
      Some(fieldType.typeArgs.head -> MemberType.Seq)
    else if (isSupertype[Option[_]](fieldType) && fieldType.typeArgs.nonEmpty && isSupertype1(
        baseType,
        fieldType.typeArgs.head))
      Some(fieldType.typeArgs.head -> MemberType.Option)
    else if (isSupertype1(baseType, fieldType))
      Some(fieldType -> MemberType.Normal)
    else if (specials.exists(s =>
        isSupertypeNoErasure(s.specialType, fieldType) && s.fieldName.fold(true)(fn => name == fn)))
      Some(fieldType -> MemberType.Special)
    else
      None

  private object MemberType extends Enumeration {
    val Seq, List, Vector, Option, Normal, Special = Value

    def coll(memberType: MemberType.Value) = memberType match {
      case Seq | List | Vector | Option => true
      case _ => false
    }
  }

  private def validateTransformations(transformations: Seq[Tree]) = transformations.map {
    case q"$setting.apply[$matchType]($enter, $leave)" if checkSetting[Visit.type](setting) =>
      Right(MacroVisit(matchType.tpe, enter, leave))

    case q"$setting.apply[$matchType, $specialType]($fn)"
        if checkSetting[VisitAnyField.type](setting) =>
      Right(MacroVisitAnyField(matchType.tpe, specialType.tpe, fn, None))

    case q"$setting.apply[$matchType, $specialType](${fieldName: String}, $fn)"
        if checkSetting[VisitAnyFieldByName.type](setting) =>
      Right(MacroVisitAnyField(matchType.tpe, specialType.tpe, fn, Some(fieldName)))

    case tree =>
      Left(
        tree.pos ->
          "Unsupported shape of transformation. Please define subclasses of `Transformation` directly in the argument list of the macro.")
  }

  def checkSetting[T: WeakTypeTag](setting: Tree) = weakTypeTag[T].tpe =:= c.typecheck(setting).tpe

  private def isSupertype[T: TypeTag](subtype: Type) =
    subtype.erasure <:< typeTag[T].tpe.erasure

  private def isSupertype1(t: Type, subtype: Type) =
    subtype.erasure <:< t.erasure

  private def isSupertypeNoErasure(t: Type, subtype: Type) =
    subtype <:< t

  private def collectKnownSubtypes(s: Symbol): Set[Symbol] = {
    s.typeSignature // triggers some side-effects (without which `isCaseClass` does not work)

    if (s.isModule || s.isModuleClass)
      Set(s)
    else if (s.isClass) {
      val cs = s.asClass

      if (cs.isCaseClass) Set(cs)
      else if ((cs.isTrait || cs.isAbstract) && cs.isSealed)
        cs.knownDirectSubclasses.flatMap(collectKnownSubtypes(_))
      else Set.empty
    } else Set.empty
  }

  def reportErrors(errors: Seq[(Position, String)]) = {
    require(errors.nonEmpty)

    val (lastPos, lastError) = errors.last

    errors.dropRight(1).foreach { case (pos, error) => c.error(pos, error) }

    c.abort(lastPos, lastError)
  }

  sealed trait MacroTransformer {
    def matchType: Type
  }

  case class MacroVisit(matchType: Type, enter: Tree, leave: Tree) extends MacroTransformer
  case class MacroVisitAnyField(
      matchType: Type,
      specialType: Type,
      fn: Tree,
      fieldName: Option[String])
      extends MacroTransformer
}
