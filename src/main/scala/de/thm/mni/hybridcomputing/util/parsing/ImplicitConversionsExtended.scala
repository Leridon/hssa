package de.thm.mni.hybridcomputing.util.parsing

import scala.util.parsing.combinator.Parsers

trait ImplicitConversionsExtended { self: Parsers =>
  implicit def flatten6[A, B, C, D, E, F, G](g: (A, B, C, D, E, F) => G): A ~ B ~ C ~ D ~ E ~F => G =
    (p: ~[~[~[~[~[A, B], C], D], E], F]) => p match {case a ~ b ~ c ~ d ~ e ~ f=> g(a, b, c, d, e, f)}
  implicit def flatten7[A, B, C, D, E, F, G, H](h: (A, B, C, D, E, F, G) => H): A ~ B ~ C ~ D ~ E ~F ~G => H =
    (p: ~[~[~[~[~[~[A, B], C], D], E], F], G]) => p match {case a ~ b ~ c ~ d ~ e ~ f ~ g=> h(a, b, c, d, e, f, g)}
}
