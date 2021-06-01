package sangria.visitor

trait VisitMacroMethods {
  inline def visit[T](inline rootNode: T, inline transformations: Transformer[_ <: T]*): T =
    ${ VisitMacro.visitCode('rootNode, 'transformations) }
}
