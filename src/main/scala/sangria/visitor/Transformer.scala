package sangria.visitor

sealed trait Transformer[T]

case class Visit[T](
  enter: T => VisitorCommand,
  leave: T => VisitorCommand = (_: T) => VisitorCommand.Continue) extends Transformer[T]

case class VisitAnyField[T, S](fn: (T, S) => VisitorCommand) extends Transformer[T]

case class VisitAnyFieldByName[T, S](fieldName: String, fn: (T, S) => VisitorCommand) extends Transformer[T]
