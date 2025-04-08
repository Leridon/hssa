package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.plugin.Basic
import de.thm.mni.hybridcomputing.util.FormattingUtilities
import de.thm.mni.hybridcomputing.util.parsing.{Positioned, SourcePosition}

object Formatting {
    extension (self: Positioned)
        def checkSource(f: String => Boolean): Boolean = {
            if (self.position == null) return false
            
            f(self.position.string)
        }
    
    private def prependSpaceIfNotEmpty(inner: String): String = {
        if (inner.isEmpty) inner
        else s" $inner"
    }
    
    def format(exp: Syntax.Expression, add_parens_to_pair: Boolean = false, make_unit_explicit: Boolean = false): String = {
        val paren_count: Int =
            if (exp.position != null) FormattingUtilities.count_parens(exp.position.string.strip())
            else if (exp.isInstanceOf[Expression.Pair] && add_parens_to_pair) 1
            else 0
        
        val inner = exp match
            case Expression.Literal(Basic.Unit) => ""
            case Expression.Literal(value) => value.toString
            case Expression.Pair(a, b) => s"${format(a, true, true)}, ${format(b, false, true)}"
            case Expression.Unit() => ""
            case Expression.Variable(name) => name.toString
            case Expression.Invert(sub) => s"~${format(sub, true, true)}"
        
        val with_parens = FormattingUtilities.parenthesize(inner, paren_count)
        
        if (with_parens.isEmpty && make_unit_explicit) "()"
        else with_parens
    }
    
    def formatInColumns(statement: Syntax.Statement): Array[String] = statement match {
        case Syntax.Assignment(target, rel, arg, consumed) =>
            Array(format(target, false, true), ":=", s"${format(rel, true, true)}${prependSpaceIfNotEmpty(format(arg))}", ":=", format(consumed, false, true))
        case Syntax.Exit(labels, argument) =>
            Array("", "", s"-> ${labels.mkString(",")}", ":=", format(argument))
        case Syntax.Entry(initialized, labels) =>
            Array(format(initialized), ":=", s"${labels.mkString(",")} <-", "", "")
    }
    
    def format(statement: Syntax.Statement): String = {
        formatInColumns(statement).mkString(" ")
    }
    
    private def center(s: String, width: Int): String = {
        val before = (width - s.length) / 2
        
        " ".repeat(before) + s + " ".repeat(width - before - s.length)
    }
    
    def format(block: Syntax.Block): String = {
        val rows = block.sequence.map(stm => (stm, formatInColumns(stm)))
        
        val column_widths = Array(0, 1, 2, 3, 4)
          .map(i => rows.map(r => r._2(i).length).max)
        
        rows.map(row => {
            row._2.zipWithIndex.map({ case (value, index) =>
                center(value, column_widths(index))
            }).mkString(" ")
        }).mkString("\n")
    }
    
    def format(rel: Syntax.Relation): String = {
        val rows = rel.blocks.flatMap(_.sequence).map(stm => (stm, formatInColumns(stm)))
        
        val column_widths = Array(0, 1, 2, 3, 4)
          .map(i => rows.map(r => r._2(i).length).max)
        
        s"rel ${rel.name}${prependSpaceIfNotEmpty(format(rel.parameter))}:\n" + rows.map(row => {
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
