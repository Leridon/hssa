package de.thm.mni.hybridcomputing.theseus

object Syntax {
    
    case class AlgebraicTypeDefinition(name: String, `type`: AlgebraicDataType)
    case class AlgebraicDataType(constructors: List[DataConstructor])
    case class DataConstructor(name: String, parameters: List[String])
    
    sealed trait Expression
    case class PairExpression(a: Expression, b: Expression) extends Expression
    case class Application(name: String, argument: Expression) extends Expression
    case class NamedExpression(name: String) extends Expression
    
    sealed trait TypeExpression
    case class NamedTypeExpression(name: String) extends TypeExpression
    case class SumType(a: TypeExpression, b: TypeExpression) extends TypeExpression
    case class ProductType(a: TypeExpression, b: TypeExpression) extends TypeExpression
    
    case class Isomorphism()
}
