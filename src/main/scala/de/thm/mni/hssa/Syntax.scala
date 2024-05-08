package de.thm.mni.hssa

object Syntax {
    abstract class Expression
    
    object Expression {
        case class Literal(value: Int) extends Expression
        case class Variable(name: String) extends Expression
        case class Pair(a: Expression, b: Expression) extends Expression
        case class Unit() extends Expression
    }
    
    abstract class Statement
    case class Assignment(
                           target: Expression,
                           inverted: Boolean,
                           relation_name: String,
                           instance_argument: Expression,
                           source: Expression
                         ) extends Statement
    
    case class UnconditionalExit(target: String, argument: Expression) extends Statement
    case class ConditionalExit(target1: String, target2: String, argument: Expression) extends Statement
    case class UnconditionalEntry(initialized: Expression, target: String) extends Statement
    case class ConditionalEntry(initialized: Expression, target1: String, target2: String) extends Statement
    
    case class Relation(name: String, parameter: Expression, body: List[Statement])
    
    case class Program(definitions: List[Relation])
}

object SyntaxExtensions {
    extension (self: Syntax.Statement)
        def isExit: Boolean = self match
            case _: Syntax.ConditionalExit => true
            case _: Syntax.UnconditionalExit => true
            case _ => false
        
        def isEntry: Boolean = self match
            case _: Syntax.ConditionalEntry => true
            case _: Syntax.UnconditionalEntry => true
            case _ => false
}