package sangria.visitor

sealed trait VisitorCommand
sealed trait VisitorControlCommand extends VisitorCommand

object VisitorCommand {
  case object Skip extends VisitorControlCommand
  case object Continue extends VisitorControlCommand
  case object Break extends VisitorControlCommand

  case class Transform[T](newValue: T, controlCommand: VisitorControlCommand = Continue)
      extends VisitorCommand
  case object Delete extends VisitorCommand
  case object DeleteAndBreak extends VisitorCommand
}
