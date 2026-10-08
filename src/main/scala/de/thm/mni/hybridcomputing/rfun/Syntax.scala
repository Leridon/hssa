package de.thm.mni.hybridcomputing.rfun

import de.thm.mni.hybridcomputing.util.parsing.{HasTokens, Positioned}
import de.thm.mni.hybridcomputing.util.reversibility.Direction

object Syntax {
    sealed trait Node extends HasTokens[Lexing.TokenClass] with Positioned

    case class Identifier(name: String) extends Node

    sealed trait Pattern extends Node
    case class TuplePattern(elements: Seq[Pattern]) extends Pattern
    case class VariablePattern(name: Identifier) extends Pattern
    case class ConsPattern(head: Pattern, tail: Pattern) extends Pattern
    case class ConstructorPattern(constructor: Identifier, arguments: List[Pattern]) extends Pattern
    case class NilPattern() extends Pattern
    case class UnitPattern() extends Pattern

    case class Assign(pattern: Pattern, function: Identifier, direction: Direction, parameter_arguments: List[Pattern], consumed_argument: Pattern) extends Node
    case class LetExpression(assigns: List[Assign]) extends Node

    sealed trait TypeExpression extends Node
    case class NamedTypeExpression(name: Identifier) extends TypeExpression
    case class ListTypeExpreesion(element_type: TypeExpression) extends TypeExpression
    case class FunTypeExpression(domain: TypeExpression, codomain: TypeExpression) extends TypeExpression
    case class BijectionTypeExpression(domain: TypeExpression, codomain: TypeExpression) extends TypeExpression
    case class TupleTypeExpression(elements: List[TypeExpression]) extends TypeExpression
    case class UnitTypeExpression() extends TypeExpression

    sealed trait Definition extends Node

    case class Constructor(name: Identifier, parameters: List[TypeExpression]) extends Node
    case class DataTypeDefinition(name: Identifier, constructors: List[Constructor]) extends Definition

    case class Case(parameter_patterns: List[Pattern], in_pattern: Pattern, body: Option[LetExpression], out_pattern: Pattern)
    case class FunctionDefinition(name: Identifier, signature: TypeExpression, cases: List[Case]) extends Definition

    case class Program(defs: List[Definition]) extends Node
}
