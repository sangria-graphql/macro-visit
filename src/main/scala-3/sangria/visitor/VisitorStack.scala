package sangria.visitor

import scala.collection.mutable.{Map => MutableMap}

class VisitorStack[T](
    var node: Option[T],
    var updated: Boolean,
    var deleted: Boolean,
    var currMember: String,
    var memberIndex: Int,
    var memberSize: Int,
    var edits: MutableMap[String, MutableMap[Int, Option[T]]],
    var specialEdits: MutableMap[String, Any],
    var prev: VisitorStack[T],
    var next: VisitorStack[T])

object VisitorStack {
  def initial[T](node: T): VisitorStack[T] =
    new VisitorStack[T](Option(node), false, false, null, -1, -1, null, null, null, null)
}
