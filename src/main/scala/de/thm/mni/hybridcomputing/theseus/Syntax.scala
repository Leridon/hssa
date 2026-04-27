package de.thm.mni.hybridcomputing.theseus

import de.thm.mni.hybridcomputing.util.parsing.{HasTokens, Positioned}

object Syntax {

    sealed trait Node extends Positioned with HasTokens[Lexing.TokenType]
    sealed trait Definition extends Node

    case class Identifier(name: String) extends Node

    case class AlgebraicDataType(name: Identifier, constructors: List[DataConstructor]) extends Definition
    case class DataConstructor(name: Identifier, parameters: Option[TypeExpression]) extends Node
    
    sealed trait Expression extends Node
    case class PairExpression(a: Expression, b: Expression) extends Expression
    case class Application(name: Identifier, argument: Expression) extends Expression
    case class VariablePattern(name: Identifier) extends Expression
    
    sealed trait TypeExpression extends Node
    case class NamedTypeExpression(name: Identifier) extends TypeExpression
    case class SumType(a: TypeExpression, b: TypeExpression) extends TypeExpression
    case class ProductType(a: TypeExpression, b: TypeExpression) extends TypeExpression

    sealed trait IsomorphismSignature extends Node
    case class BijectionType(a: TypeExpression, b: TypeExpression) extends IsomorphismSignature
    case class ParameterizedBijectionType(parameter: TypeExpression, body: BijectionType) extends IsomorphismSignature

    case class Isomorphism(name: Identifier, signature: IsomorphismSignature, cases: Seq[Case]) extends Definition
    case class Case(left: Expression, right: Expression) extends Node


    case class Program(definitions: Seq[Definition]) extends Node
}
