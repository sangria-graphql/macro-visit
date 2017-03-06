package sangria

import language.experimental.{macros ⇒ `scalac, please just let me do it!`}

package object visitor {
  def visit[T](rootNode: T, transformations: Transformer[_ <: T]*): T = macro VisitMacro.visitImpl[T]
}
