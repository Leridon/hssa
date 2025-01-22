package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.plugin.Basic
import de.thm.mni.hybridcomputing.util.parsing.{Positioned, SourcePosition}

object Formatting {
    
    extension (self: Positioned)
        def checkSource(f: String => Boolean): Boolean = {
            if (self.position == null) return false
            
            f(self.position.string)
        }
    
    private def formatExpressionWithOptionalSpace(exp: Syntax.Expression): String = {
        val inner = format(exp)
        
        if (inner.isEmpty) inner
        else s" $inner"
    }
    
    def format(exp: Syntax.Expression, force_parens: Boolean = false): String = {
        val inner = exp match
            case Expression.Literal(Basic.Unit) => ""
            case Expression.Literal(value) => value.toString
            case Expression.Pair(a, b) => s"${format(a)}, ${format(b)}"
            case Expression.Unit() => ""
            case Expression.Variable(name) => name.toString
            case Expression.Invert(sub) => s"~${format(sub)}"
        
        if (force_parens || exp.checkSource(s => s.startsWith("(") && s.endsWith(")"))) s"($inner)"
        else inner
    }
    
    def formatInColumns(statement: Syntax.Statement): Array[String] = statement match {
        case Syntax.Assignment(target, rel, arg, consumed) =>
            Array(format(target), ":=", s"${format(rel)}${formatExpressionWithOptionalSpace(arg)}", ":=", format(consumed))
        case Syntax.Exit(labels, argument) =>
            Array("", "", s"-> ${labels.mkString(",")}", ":=", format(argument))
        case Syntax.Entry(initialized, labels) =>
            Array(format(initialized), ":=", s"${labels.mkString(",")} <-", "", "")
    }
    
    def format(statement: Syntax.Statement): String = {
        formatInColumns(statement).mkString(" ")
    }
    
    def format(rel: Syntax.Relation): String = {
        val rows = rel.blocks.flatMap(_.sequence).map(stm => (stm, formatInColumns(stm)))
        
        val column_widths = Array(0, 1, 2, 3, 4)
          .map(i => rows.map(r => r._2(i).length).max)
        
        def center(s: String, width: Int): String = {
            val before = (width - s.length) / 2
            
            " ".repeat(before) + s + " ".repeat(width - before - s.length)
        }
        
        s"rel ${rel.name}${formatExpressionWithOptionalSpace(rel.parameter)}:\n" + rows.map(row => {
            val r = row._2.zipWithIndex.map({ case (value, index) =>
                center(value, column_widths(index))
            }).mkString(" ")
            
            if (row._1.isExit) r + "\n" else r
            
        }).mkString("\n")
    }
    
    def format(prog: Syntax.Program): String = {
        prog.definitions.map(this.format).mkString("\n\n")
    }
}
