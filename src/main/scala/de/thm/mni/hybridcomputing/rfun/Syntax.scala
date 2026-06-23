package de.thm.mni.hybridcomputing.rfun

import de.thm.mni.hybridcomputing.util.parsing.{HasTokens, Positioned}
import de.thm.mni.hybridcomputing.util.reversibility.Direction

object Syntax {
    sealed trait Node extends HasTokens[Lexing.TokenClass] with Positioned

    case class Identifier(name: String) extends Node

    sealed trait Pattern extends Expression
    case class TuplePattern(elements: Seq[Pattern]) extends Pattern
    case class VariablePattern(name: Identifier) extends Pattern
    case class ConsPattern(head: Pattern, tail: Pattern) extends Pattern
    case class ConstructorPattern(constructor: Identifier, arguments: List[Pattern]) extends Pattern
    case class NilPattern() extends Pattern

    sealed trait Expression extends Node

    case class Assign(pattern: Pattern, function: Identifier, direction: Direction, arguments: List[Pattern]) extends Node
    case class LetExpression(assigns: List[Assign], in_pattern: Pattern) extends Expression

    sealed trait TypeExpression extends Node
    case class NamedTypeExpression(name: Identifier) extends TypeExpression
    case class ListTypeExpreesion(element_type: TypeExpression) extends TypeExpression
    case class FunTypeExpression(domain: TypeExpression, codomain: TypeExpression) extends TypeExpression
    case class BijectionTypeExpression(domain: TypeExpression, codomain: TypeExpression) extends TypeExpression

    sealed trait Definition extends Node

    case class Constructor(name: Identifier, parameters: List[TypeExpression]) extends Node
    case class DataTypeDefinition(name: Identifier, constructors: List[Constructor]) extends Definition

    case class Case(parameters: List[Pattern], body: Expression)
    case class FunctionDefinition(name: Identifier, signature: TypeExpression, cases: List[Case]) extends Definition

    case class Program(defs: List[Definition]) extends Node
}
