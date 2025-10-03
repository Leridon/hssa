package de.thm.mni.hybridcomputing.roopl

import de.thm.mni.hybridcomputing.roopl.Syntax.*
import de.thm.mni.hybridcomputing.roopl.Syntax.Statement.*
import de.thm.mni.hybridcomputing.roopl.Syntax.Operator.*
import de.thm.mni.hybridcomputing.roopl.Syntax.Expression.Literal
import de.thm.mni.hybridcomputing.roopl.Syntax.Expression.Reference
import de.thm.mni.hybridcomputing.roopl.Syntax.Expression.Binary
import de.thm.mni.hybridcomputing.roopl.Syntax.ObjectType.IntegerArray
import de.thm.mni.hybridcomputing.roopl.Syntax.ObjectType.ClassArray

object Formatting {
    case class Options(parenthesizeExpressions: Boolean = false, indentBy: Int = 4)

    def format(prog: Program): String = {
        Formatting(Options()).format(prog)
    }
}

class Formatting(options: Formatting.Options = Formatting.Options()) {

    val spaces = (" " * options.indentBy)
    val tab = spaces * (_: Int)
    
    def format(prog: Program): String = {
        prog.definitions.map(format).mkString("\n\n")
    }
    
    def format(classDefinition: ClassDefinition): String = {
        val inherits = classDefinition.inherits.map(inherit => s"inherits ${inherit}").getOrElse("")
        s"class ${classDefinition.name} ${inherits}\n" +
          classDefinition.variableDefinitions.map(tab(1) + format(_)).mkString("\n") +
          "\n\n" +
          classDefinition.methodDefinitions.map(format).mkString("\n\n")
    }
    
    def format(variableDefinition: VariableDefinition): String = {
        s"${variableDefinition.typ} ${variableDefinition.name}"
    }
    
    def format(methodDefinition: MethodDefinition): String = {
        tab(1) + s"method ${methodDefinition.name}(${methodDefinition.parameters.map(format).mkString(", ")})\n" +
          format(methodDefinition.body, 2)
    }
    
    def format(statement: Statement, indent: Int): String = {
        (if !statement.isInstanceOf[Block] then tab(indent) else "") + (statement match
            case Assignment(assignee, op, value) => s"${format(assignee)} ${format(op)} ${format(value)}"
            case Swap(left, right) => s"${format(left)} <=> ${format(right)}"
            // Could add a case for else skip to not print skip in a new line
            case Conditional(test, thenStatement, elseStatement, assertion) => s"if ${format(test)} then\n${format(thenStatement, indent + 1)}\n${tab(indent)}else\n${format(elseStatement, indent + 1)}\n${tab(indent)}fi ${format(assertion)}"
            case Loop(test, doStatement, loopStatement, assertion) => s"from ${format(test)} do\n${format(doStatement, indent + 1)}\n${tab(indent)}loop\n${format(loopStatement, indent + 1)}\n${tab(indent)}until ${format(assertion)}" 
            case ObjectBlock(typ, name, body) => s"construct $typ $name\n${format(body, indent + 1)}\n${tab(indent)}destruct $name"
            case LocalBlock(initType, initName, compute, statement, uncompute) => s"local $initType $initName = ${format(compute)}\n${format(statement, indent)}\n${tab(indent)}delocal $initType $initName = ${format(uncompute)}"
            case New(typ, name) => s"new ${format(typ)} ${format(name)}"
            case Delete(typ, name) => s"delete ${format(typ)} ${format(name)}"
            case Copy(typ, from, to) => s"copy ${format(typ)} ${format(from)} ${format(to)}"
            case Uncopy(typ, from, to) => s"uncopy ${format(typ)} ${format(from)} ${format(to)}"
            case CallLocal(method, args) => s"call $method(${args.mkString(", ")})"
            case UncallLocal(method, args) => s"uncall $method(${args.mkString(", ")})"
            case Call(callee, method, args) => s"call ${format(callee)}::$method(${args.mkString(", ")})"
            case Uncall(callee, method, args) => s"uncall ${format(callee)}::$method(${args.mkString(", ")})"
            case Skip => "skip"
            // Could tell statements in the list whether they're the first in the block and, if not, have conditionals, loops etc. print newlines before and after themselves
            case Block(list) => list.map(format(_, indent)).mkString("\n")
          )
    }
    
    def format(objectType: ObjectType): String = {
        objectType match
            case ObjectType.Class(name) => name.toString
            case ObjectType.IntegerArray(size) => s"int[${format(size)}]"
            case ObjectType.ClassArray(name, size) => s"$name${format(size)}"
    }
    
    def format(variableReference: VariableReference): String = {
        variableReference match
            case VariableReference.Variable(name) => name.toString
            case VariableReference.Array(name, index) => s"${name}[${format(index)}]"
    }
    
    private def parenthesize(string: String): String = {
        s"(${string})"
    }
    
    def format(expression: Expression, parenthesizeExpressions: Boolean = false): String = {
        expression match
            case Literal(value) => value.toString()
            case Reference(reference) => format(reference)
            case Expression.Nil => "nil"
            case Binary(left, op, right) => {
                // Must parenthesize subexpressions if their operator has lower or equal precedence than this.op
                // Since all operators are left-associative parenthesizing the left expression is not required with equal precedence
                // Otherwise associativity would have to be taken into account as well
                val leftString = format(left, options.parenthesizeExpressions || left.isInstanceOf[Binary] && precedence(op) > precedence(left.asInstanceOf[Binary].op))
                val rightString = format(right, options.parenthesizeExpressions || right.isInstanceOf[Binary] && precedence(op) >= precedence(right.asInstanceOf[Binary].op))
                val expressionString = s"${leftString} ${format(op)} ${rightString}"
                
                if (parenthesizeExpressions) parenthesize(expressionString)
                else expressionString
            }
    }
    
    private def precedence(operator: Operator): Int = {
        operator match
            case LOGOR => 1
            case LOGAND => 2
            case BITOR => 3
            case XOR => 4
            case BITAND => 5
            case EQUAL => 6
            case NOTEQUAL => 7
            case LESSTHAN => 8
            case GREATERTHAN => 8
            case LESSEQUAL => 8
            case GREATEREQUAL => 8
            case ADD => 9
            case SUB => 9
            case MUL => 10
            case DIV => 10
            case MOD => 10
    }
    
    def format(operator: Operator): String = {
        operator match
            case ADD => "+"
            case SUB => "-"
            case XOR => "^"
            case MUL => "*"
            case DIV => "/"
            case MOD => "%"
            case BITAND => "&"
            case BITOR => "|"
            case LOGAND => "&&"
            case LOGOR => "||"
            case LESSTHAN => "<"
            case GREATERTHAN => ">"
            case EQUAL => "="
            case NOTEQUAL => "!="
            case LESSEQUAL => "<="
            case GREATEREQUAL => ">="
    }
    
    def format(operator: AssignmentOperator): String = {
        operator match
            case AssignmentOperator.ADD => "+="
            case AssignmentOperator.SUB => "-="
            case AssignmentOperator.XOR => "^="
    }
}