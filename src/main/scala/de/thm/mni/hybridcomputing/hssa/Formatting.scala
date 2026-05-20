package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Formatting.Alignment.LEFT
import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.util.FormattingUtilities
import de.thm.mni.hybridcomputing.util.parsing.{Positioned, SourcePosition}

object Formatting {
    enum Alignment {
        case LEFT
        case LEFT_WITH_INVERSION
        case RIGHT
        case CENTER
    }

    case class ColumnWidth(width: Int, account_for_inversion: Boolean)
    object ColumnWidth {
        def fromColumn(col: Seq[Cell]): ColumnWidth = {
            val account_for_inversion = col.exists(_.alignment == Alignment.LEFT_WITH_INVERSION) && col.exists(_.content.startsWith("~"))

            val width = col.map(cell => if (account_for_inversion && !cell.content.startsWith("~")) cell.content.length + 1 else cell.content.length).max

            ColumnWidth(width, account_for_inversion)
        }
    }

    case class Cell(content: String, alignment: Alignment) {
        def asString(col_width: ColumnWidth): String = alignment match {
            case Alignment.LEFT => content.padTo(col_width.width, ' ')
            case Alignment.RIGHT => " ".repeat(col_width.width - content.length) + content
            case Alignment.CENTER =>
                val spaces = col_width.width - content.length
                val after = spaces / 2
                val before = spaces - after

                " ".repeat(before) + content + " ".repeat(after)
            case Alignment.LEFT_WITH_INVERSION =>
                if (col_width.account_for_inversion && !content.startsWith("~")) {
                    " " + content.padTo(col_width.width - 1, ' ')
                } else {
                    content.padTo(col_width.width, ' ')
                }
        }
    }

    val ASGN = Cell(":=", Alignment.CENTER)
    val NGSA = Cell("=:", Alignment.CENTER)
    val EMPTY = Cell("", Alignment.CENTER)

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
            case Expression.Literal(value) => value.toString
            case Expression.Pair(a, b) => s"${format(a, true, true)}, ${format(b, false, true)}"
            case Expression.Unit() => ""
            case Expression.Wildcard() => "*"
            case Expression.Variable(name) => name.toString
            case Expression.Duplicate(name) => s"'${format(name, true, true)}"
            case Expression.Invert(sub) => s"~${format(sub, true, true)}"
            case Expression.Application(rel, p, in) => s"[${format(rel, true, true)} ${format(p, true, true)} ${format(in, true, true)}]"

        val with_parens = FormattingUtilities.parenthesize(inner, paren_count)

        if (with_parens.isEmpty && make_unit_explicit) "()"
        else with_parens
    }

    def formatInColumns(statement: Syntax.Statement): Array[Cell] = statement match {
        case Syntax.Assignment(target, rel, arg, consumed) =>
            Array(Cell(format(target, false, true), Alignment.LEFT), ASGN, Cell(s"${format(rel, true, true)}${prependSpaceIfNotEmpty(format(arg))}", Alignment.LEFT_WITH_INVERSION), NGSA, Cell(format(consumed, false, true), LEFT))
        case Syntax.Exit(labels, argument) =>
            Array(EMPTY, EMPTY, Cell(s"-> ${labels.mkString(",")}", Alignment.RIGHT), NGSA, Cell(format(argument), Alignment.LEFT))
        case Syntax.Entry(initialized, labels) =>
            Array(Cell(format(initialized), Alignment.LEFT), ASGN, Cell(s"${labels.mkString(",")} <-", Alignment.LEFT), EMPTY, EMPTY)
    }

    def format(statement: Syntax.Statement): String = {
        formatInColumns(statement).mkString(" ")
    }

    def format(block: Syntax.Block): String = {
        val rows = block.sequence.map(stm => formatInColumns(stm))

        val column_widths = Array(0, 1, 2, 3, 4)
          .map(i => ColumnWidth.fromColumn(rows.map(r => r(i))))

        rows.map(row => {
            row.zipWithIndex.map({ case (cell, index) =>
                cell.asString(column_widths(index))
            }).mkString(" ")
        }).mkString("\n")
    }

    def format(rel: Syntax.Relation): String = {
        val rows = rel.blocks.flatMap(_.sequence).map(stm => (stm, formatInColumns(stm)))

        val column_widths = Array(0, 1, 2, 3, 4)
          .map(i => ColumnWidth.fromColumn(rows.map(r => r._2(i))))

        s"rel ${rel.name}${prependSpaceIfNotEmpty(format(rel.parameter))}:\n" + rows.map(row => {
            val r = "  " + row._2.zipWithIndex.map({ case (cell, index) =>
                cell.asString(column_widths(index))
            }).mkString(" ")

            if (row._1.isExit) r + "\n" else r

        }).mkString("\n")
    }

    def format(prog: Syntax.Program): String = {
        prog.definitions.map(this.format).mkString("\n")
    }
}
