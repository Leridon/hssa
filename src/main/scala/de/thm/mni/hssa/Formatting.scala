package de.thm.mni.hssa

import de.thm.mni.hssa.Syntax.Expression
import de.thm.mni.hssa.SyntaxExtensions._

object Formatting {
    
    def format(exp: Syntax.Expression): String = {
        exp match
            case Expression.Literal(value) => value.toString
            case Expression.Pair(a, b) => s"(${format(a)}, ${format(b)})"
            case Expression.Unit() => "()"
            case Expression.Variable(name) => name
            case Expression.Inversion(sub) => s"~${format(sub)}"
    }
    
    def formatInColumns(statement: Syntax.Statement): Array[String] = statement match {
        case Syntax.Assignment(target, rel, arg, consumed) =>
            Array(format(target), ":=", s"${format(rel)} ${format(arg)}", ":=", format(consumed))
        case Syntax.UnconditionalEntry(initialized, target) =>
            Array(format(initialized), ":=", s"$target <-", "", "")
        case Syntax.ConditionalEntry(initialized, target1, target2) =>
            Array(format(initialized), ":=", s"$target1,$target2 <-", "", "")
        case Syntax.UnconditionalExit(target, argument) =>
            Array("", "", s"-> $target", ":=", format(argument))
        case Syntax.ConditionalExit(target1, target2, argument) =>
            Array("", "", s"-> $target1,$target2", ":=", format(argument))
    }
    
    def format(statement: Syntax.Statement): String = {
        formatInColumns(statement).mkString(" ")
    }
    
    def format(rel: Syntax.Relation): String = {
        type A = Array[String]
        
        val rows = rel.body.map(stm => (stm, formatInColumns(stm)))
        
        val column_widths = Array(0, 1, 2, 3, 4)
          .map(i => rows.map(r => r._2(i).length).max)
        
        def center(s: String, width: Int): String = {
            val before = (width - s.length) / 2
            
            " ".repeat(before) + s + " ".repeat(width - before - s.length)
        }
        
        s"rel ${rel.name} (${format(rel.parameter)})\n" + rows.map(row => {
            val r = row._2.zipWithIndex.map({ case (value, index) =>
                center(value, column_widths(index))
            }).mkString(" ")
            
            if(row._1.isExit) r + "\n" else r
            
        }).mkString("\n")
    }
    
    def format(prog: Syntax.Program): String = {
        prog.definitions.map(this.format).mkString("\n\n")
    }
}
