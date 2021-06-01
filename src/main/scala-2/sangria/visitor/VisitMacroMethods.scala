package sangria.visitor

import language.experimental.{macros => `scalac, please just let me do it!`}

trait VisitMacroMethods {
  def visit[T](rootNode: T, transformations: Transformer[_ <: T]*): T =
    macro VisitMacro.visitImpl[T]
}
