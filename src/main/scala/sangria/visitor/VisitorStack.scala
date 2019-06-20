package sangria.visitor

import scala.collection.mutable.{Map => MutableMap}

class VisitorStack[T](
  var node: T,
  var updated: Boolean,
  var deleted: Boolean,
  var currMember: String,
  var memberIndex: Int,
  var memberSize: Int,
  var edits: MutableMap[String, MutableMap[Int, T]],
  var specialEdits: MutableMap[String, Any],
  var prev: VisitorStack[T],
  var next: VisitorStack[T])
