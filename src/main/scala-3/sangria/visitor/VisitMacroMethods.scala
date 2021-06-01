package sangria.visitor

trait VisitMacroMethods {
  inline def visit[T](inline rootNode: T, inline transformations: Transformer[_ <: T]*): T =
    ${ VisitMacro.visitCode('rootNode, 'transformations) }

  inline def visit2[T](inline rootNode: T, inline transformations: Int*): T =
    ${ VisitMacro.visitCode2('rootNode, 'transformations) }
}
