package de.thm.mni.hybridcomputing.hssa.util

import de.thm.mni.hybridcomputing.hssa.Language.Plugin
import de.thm.mni.hybridcomputing.hssa.Syntax
import de.thm.mni.hybridcomputing.hssa.plugin.Basic

import scala.annotation.targetName
import scala.language.implicitConversions

object HssaDSL {
    case class IncompleteApplication(target: Syntax.Expression, rel: Syntax.Expression, par: Syntax.Expression) {
        def :=(source: Syntax.Expression): Syntax.Assignment = Syntax.Assignment(target, rel, par, source)
    }
    
    case class IncompleteExit(labels: Seq[Syntax.Identifier]) {
        def :=(source: Syntax.Expression): Syntax.Exit = Syntax.Exit(labels, source)
    }
    
    def ->(labels: Seq[String]): IncompleteExit = IncompleteExit(labels.map(Syntax.Identifier.apply))
    @targetName("incompleteExitFromVarArgs")
    def ->(labels: String*): IncompleteExit = IncompleteExit(labels.map(Syntax.Identifier.apply))
    
    extension (self: Syntax.Expression) {
        def :=<-(labels: Seq[String]): Syntax.Entry = {
            Syntax.Entry(self, labels.map(l => Syntax.Identifier(l)))
        }
        
        @targetName("addLabels")
        def :=<-(labels: String*): Syntax.Entry = self :=<- labels
        
        def :==(rel_par: (Syntax.Expression, Syntax.Expression)): IncompleteApplication = IncompleteApplication(self, rel_par._1, rel_par._2)
        def :=(rel: Syntax.Expression): IncompleteApplication = IncompleteApplication(self, rel, Syntax.Expression.Unit())
        
        def unary_~ : Syntax.Expression.Invert = Syntax.Expression.Invert(self)
    }
    
    trait Expressionable[T]:
        def toExpression(v: T): Syntax.Expression
    
    given Expressionable[Unit] with
        def toExpression(v: Unit): Syntax.Expression = Syntax.Expression.Literal(Basic.Unit)
    
    given Expressionable[String] with
        def toExpression(v: String): Syntax.Expression = Syntax.Expression.Variable(Syntax.Identifier(v))
    
    given Expressionable[Int] with
        def toExpression(v: Int): Syntax.Expression = Syntax.Expression.Literal(Basic.Int(v))
    
    given Expressionable[Syntax.Expression] with
        def toExpression(v: Syntax.Expression): Syntax.Expression = v
    
    given Expressionable[Plugin.Builtin] with
        def toExpression(v: Plugin.Builtin): Syntax.Expression = Syntax.Expression.Variable(Syntax.Identifier(v.value.name))
    
    given [A, B](using a: Expressionable[A], b: Expressionable[B]): Expressionable[(A, B)] with
        def toExpression(v: (A, B)): Syntax.Expression = Syntax.Expression.Pair(a.toExpression(v._1), b.toExpression(v._2))
    
    implicit def exp[T](v: T)(using tc: Expressionable[T]): Syntax.Expression = tc.toExpression(v)
    
    implicit def blockfromtuple(v: (Syntax.Entry, Syntax.Exit)): Syntax.Block = Syntax.Block(v._1, Seq(), v._2)
    implicit def blockfromtuple(v: (Syntax.Entry, Syntax.Assignment, Syntax.Exit)): Syntax.Block = Syntax.Block(v._1, Seq(v._2), v._3)
    
    def block(entry: Syntax.Entry, exit: Syntax.Exit): Syntax.Block = Syntax.Block(entry, Seq(), exit)
    def block(entry: Syntax.Entry, a0: Syntax.Assignment, exit: Syntax.Exit): Syntax.Block = Syntax.Block(entry, Seq(a0), exit)
    def block[T: AsAssignmentSeq](entry: Syntax.Entry, a0: T, exit: Syntax.Exit): Syntax.Block = Syntax.Block(entry, a0.toSeq, exit)
    
    
    trait AsAssignmentSeq[T]:
        extension(v: T)
            def toSeq: Seq[Syntax.Assignment]
        
    given AsAssignmentSeq[Syntax.Assignment] with
        extension(v: Syntax.Assignment)
            def toSeq: Seq[Syntax.Assignment] = Seq(v)
        
    given [T](using t: AsAssignmentSeq[T]): AsAssignmentSeq[Seq[T]] with
        extension(v: Seq[T])
            def toSeq: Seq[Syntax.Assignment] = v.flatMap(_.toSeq)
    
    implicit def assignmentseq[T: AsAssignmentSeq](v: T): Syntax.Expression = v.toSeq
}
