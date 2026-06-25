package de.thm.mni.hybridcomputing.rfun

import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.Language
import de.thm.mni.hybridcomputing.hssa.util.HssaDSL.*
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.string2ident
import de.thm.mni.hybridcomputing.hssa.util.{ProgramBuilder, RelationBuilder}
import de.thm.mni.hybridcomputing.rfun

object Translation {

    import hssa.util.HssaDSL.*

    def mangle(name: String): String = s"rfun.${name}"

    /**
     * Each pattern is a parameterless relation that maps a matchee to either (1, matchee) if it does not match, or (0, (a, b, c...)) if it matches, where a, b, c, ... are the bound variables.
     */
    def translatePattern(builder: ProgramBuilder, pattern: rfun.Syntax.Pattern): hssa.Syntax.Relation = {
        val rel = builder.newRelation(
            builder.relation_name_generator.next("pattern"), ()
        )




        pattern match {
            case Syntax.TuplePattern(elements) =>
                // Reduce to list pattern.
                // (a, b, c) => a : b : c : []


            case Syntax.VariablePattern(name) =>
                // Always succeed pattern. x => (1, x)

                rel.add(block(
                    ("m", 0) :=<-("begin"),
                    ->("end") := ((1, "m"), 0)
                ))
            case Syntax.NilPattern() =>
                // Reduce to constructor pattern with builtin constructor id 0 and () arguments
            case Syntax.ConsPattern(head, tail) =>
                // Reduce to constructor pattern with builtin constructor id 1 with (head, tail) arguments
            case Syntax.ConstructorPattern(constructor, arguments) => {
                // check if type id matches constructor id
                // No  => No match
                // Yes => Match tuple pattern
            }
        }

        rel.compile()
    }

    def extractRightHandPattern(exp: Syntax.Expression): Syntax.Pattern = {
        exp match {
            case pattern: Syntax.Pattern => pattern
            case Syntax.LetExpression(assigns, pattern) => pattern
        }
    }

    /**
     * A function matches to a relation that takes all anciallaries as parameters. They are copied into local variables for matching purposes.
     */
    def translateFunction(program_builder: hssa.util.ProgramBuilder, function: Syntax.FunctionDefinition): hssa.Syntax.Relation = {
        val builder = program_builder.newRelation(mangle(function.name.name), ())

        // Each case is a branch on the left-hand pattern and a join of the right-hand pattern.
        // It has exactly one block for the matching case, which receives all bound variables of the left-hand pattern as input
        // and passes all bound variables of the right hand pattern as output.
        // The non-matching case goes to the next case. If there is none left, an error block is added.

        def helper(cases: List[Syntax.Case], in_label: String, out_label: String): Unit = {
            cases match {
                case head :: next =>
                    val match_label_1 = builder.label_generator.next("L")
                    val match_label_2 = builder.label_generator.next("L")

                    val nomatch_label_1 = builder.label_generator.next("L")
                    val nomatch_label_2 = builder.label_generator.next("L")

                    val in_pattern = Syntax.TuplePattern(head.parameters)
                    val out_pattern = Syntax.TuplePattern(head.parameters.init :+ extractRightHandPattern(head.body))

                    val translated_in_pattern = translatePattern(program_builder, in_pattern)
                    val translated_out_pattern = translatePattern(program_builder, out_pattern)

                    builder.add(
                        block(
                            ("matchee", 0) := <--(in_label),
                            Seq(
                                ("is_match", "match_data") := translated_in_pattern.name := "matchee"
                            ),
                            ->(match_label_1, nomatch_label_1) := ("match_data", "is_match")
                        )
                    )

                    helper(next, nomatch_label_1, nomatch_label_2)

                    builder.add(
                        block(
                            ("match_data", "was_match") := <--(match_label_2, nomatch_label_2),
                            Seq(
                                "result" := ~translated_out_pattern.name := ("was_match", "match_data")
                            ),
                            ->(out_label) := ("result", 0)
                        )
                    )
                case Nil =>
            }
        }

        val in_label = builder.label_generator.next("L")
        val out_label = builder.label_generator.next("L")

        //TODO: Entry block

        helper(function.cases, in_label, out_label)

        //TODO: Exit block

        builder.compile()
    }

    /**
     * Each constructor maps to a bijection that wraps the arguments in a tuple with the constructor id
     */
    def translateType(builder: hssa.util.ProgramBuilder, typ: Syntax.DataTypeDefinition): Seq[hssa.Syntax.Relation] = {
        typ.constructors.zipWithIndex.map((constructor, i) => {

            val rel = builder.newRelation(mangle(constructor.name.name), ())
            rel.add(
                block(
                    ("args", 0) := <--("begin"),
                    ->("end") := ((i, "args"), 0),
                ))
            rel.compile()
        })
    }

    def translateProgram(program: Syntax.Program): hssa.Syntax.Program = {
        val builder = new ProgramBuilder(Language.Canon)

        program.defs.foreach {
            case t: Syntax.DataTypeDefinition => translateType(builder, t)
            case f: Syntax.FunctionDefinition => translateFunction(builder, f)
        }

        builder.compile()
    }
}
