package de.thm.mni.hybridcomputing.rfun

import de.thm.mni.hybridcomputing.hssa.Language
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.string2ident
import de.thm.mni.hybridcomputing.hssa.util.{IncrementalBlockBuilder, ProgramBuilder}
import de.thm.mni.hybridcomputing.rfun.Syntax.Pattern
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import de.thm.mni.hybridcomputing.util.reversibility
import de.thm.mni.hybridcomputing.util.reversibility.Direction
import de.thm.mni.hybridcomputing.{hssa, rfun}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Translation {

    import hssa.util.HssaDSL.*


    def mangle_id(name: String): String = name.replace("'", ".")

    def mangle(name: String): String = s"rfun.${mangle_id(name)}"


    sealed trait LinearPattern

    case class LinearPatternConstructor(constructor_id: Int) extends LinearPattern
    case class LinearPatternTuple(arity: Int) extends LinearPattern
    case object LinearPatternVariable extends LinearPattern

    def runtime_library: Seq[hssa.Syntax.Relation] = Seq(
        hssa.Syntax.Relation(hssa.Syntax.Identifier("is_instance"), (("is_id", "val"), "should_id"), Seq(
            block(
                ((), 0) := <--("begin"),
                "c" :== ("equal", ("is_id", "should_id")) := (),
                ->("end") := ("c", 0)
            )
        ))
    )

    case class ForProgram(program: Syntax.Program) {
        val program_builder = new ProgramBuilder(Language.Canon)

        def linearize(pattern: Pattern): List[LinearPattern] = {
            pattern match {
                case Syntax.TuplePattern(elements) =>
                    LinearPatternTuple(elements.length) :: elements.flatMap(linearize).toList
                case Syntax.VariablePattern(name) =>
                    LinearPatternVariable :: Nil
                case Syntax.ConsPattern(head, tail) =>
                    LinearPatternConstructor(1) :: LinearPatternTuple(2) :: List(head, tail).flatMap(linearize)
                case Syntax.ConstructorPattern(constructor, arguments) =>
                    val constructor_id = program.defs.find(d => d.isInstanceOf[Syntax.DataTypeDefinition])
                      .get.asInstanceOf[Syntax.DataTypeDefinition].constructors.indexWhere(c => c.name.name == constructor.name)

                    LinearPatternConstructor(constructor_id) :: LinearPatternTuple(arguments.length) :: arguments.flatMap(linearize)
                case Syntax.NilPattern() => LinearPatternConstructor(0) :: LinearPatternTuple(0) :: Nil
                case Syntax.UnitPattern() => LinearPatternTuple(0) :: Nil
            }
        }


        /**
         * Each pattern is a parameterless relation that maps a matchee to either (1, matchee) if it does not match, or (0, (a, b, c...)) if it matches, where a, b, c, ... are the bound variables.
         */
        def translatePattern(pattern: rfun.Syntax.Pattern): hssa.Syntax.Relation = {
            val rel = program_builder.newRelation(
                program_builder.relation_name_generator.next("pattern"), ()
            )

            @tailrec
            def helper(lin_pat: List[LinearPattern], in_label: String, out_label: String, match_label: String): Unit = {
                lin_pat match {
                    case head :: next =>
                        val match_label_forwards1 = rel.label_generator.next("continueA")
                        val match_label_forwards2 = rel.label_generator.next("continueB")
                        val inner_label = rel.label_generator.next("fail")
                        val nomatch_label_back1 = rel.label_generator.next("unravelA")
                        val nomatch_label_back2 = rel.label_generator.next("unravelB")

                        val MATCHEE_VARIABLE = "matchee"
                        val CONDITION_VARIABLE = "is_match"
                        val STATE_VARIABLE_BEFORE = "state.0"
                        val STATE_VARIABLE_AFTER = "state.1"

                        case class LinearPatternCode(
                                                      condition: Seq[hssa.Syntax.Assignment],
                                                      unpack: Seq[hssa.Syntax.Assignment]
                                                    )

                        val code: LinearPatternCode = head match {
                            case LinearPatternConstructor(constructor_id) =>

                                LinearPatternCode(
                                    // To check whether a value matches a constructor, we use a utility function
                                    Seq(CONDITION_VARIABLE :== ("is_instance", (MATCHEE_VARIABLE, constructor_id)) := ()),
                                    // To unpack a constructor, we deduplicate the constructor and place the contained
                                    // value in front of the matchee list
                                    Seq(
                                        (((constructor_id, "value"), "rest"), "matches") := "id" := STATE_VARIABLE_BEFORE,
                                        STATE_VARIABLE_AFTER := "id" := (("value", "rest"), "matches"),
                                    )
                                )
                            case LinearPatternTuple(arity) =>
                                val els = Range(0, arity).map(i => hssa.Syntax.Expression.Variable(hssa.Syntax.Identifier(s"el$i")))

                                val folded = (e: hssa.Syntax.Expression) => els.foldRight(e)(hssa.Syntax.Expression.Pair.apply)

                                LinearPatternCode(
                                    Seq(CONDITION_VARIABLE := "id" := 1),
                                    Seq(
                                        ((els.reduceRightOption(hssa.Syntax.Expression.Pair.apply).getOrElse(hssa.Syntax.Expression.Unit()), "rest"), "matches") := "id" := STATE_VARIABLE_BEFORE,
                                        STATE_VARIABLE_AFTER := "id" := (folded("rest"), "matches"),
                                    )
                                )
                            case LinearPatternVariable =>
                                LinearPatternCode(
                                    Seq(CONDITION_VARIABLE := "id" := 1),
                                    Seq(
                                        ((MATCHEE_VARIABLE, "rest"), "matches") := "id" := STATE_VARIABLE_BEFORE,
                                        STATE_VARIABLE_AFTER := "id" := ("rest", (MATCHEE_VARIABLE, "matches")),
                                    )
                                )

                        }

                        // Variable patterns always match, so there is no condition to check
                        // We still generate this block with a constant label index to stick with out translation scheme
                        // The 'inner_label' is unreachable, and can be removed by control flow optimization
                        val branch_block = block(
                            (((MATCHEE_VARIABLE, "rest"), "matches"), 0) := <--(in_label),
                            code.condition,
                            ->(inner_label, match_label_forwards1) := (((MATCHEE_VARIABLE, "rest"), "matches"), CONDITION_VARIABLE)
                        )

                        // This block is entered iff the pattern matches. It applies the effects of the pattern
                        val match_block_forwards = block(
                            (STATE_VARIABLE_BEFORE, 0) := <--(match_label_forwards1),
                            code.unpack,
                            ->(match_label_forwards2) := (STATE_VARIABLE_AFTER, 0)
                        )

                        // This block is entered when in the unraveling direction when this pattern matched, but a subsequent
                        // pattern did not. It undoes the pattern effects.
                        val match_block_backwards = block(
                            (STATE_VARIABLE_AFTER, 0) := <--(nomatch_label_back2),
                            hssa.Inversion.Local.invert(code.unpack),
                            ->(nomatch_label_back1) := (STATE_VARIABLE_BEFORE, 0)
                        )

                        // The join block is the inverse of the branch block. It uses the same condition to uncompute the branching bit
                        val join_block = block(
                            (((MATCHEE_VARIABLE, "rest"), "matches"), CONDITION_VARIABLE) := <--(inner_label, nomatch_label_back1),
                            hssa.Inversion.Local.invert(code.condition),
                            ->(out_label) := (((MATCHEE_VARIABLE, "rest"), "matches"), 0)
                        )

                        rel.add(branch_block)
                        rel.add(match_block_forwards)
                        rel.add(match_block_backwards)
                        rel.add(join_block)

                        helper(next, match_label_forwards2, nomatch_label_back2, match_label)
                    case Nil =>

                        // No more patterns are left.
                        // Assert the matchee list away by assigning to unit, and pass the matches to the final label
                        // The out_label is unreachable at this point, but included for wellformedness
                        rel.add(block(
                            (((), "matches"), 0) := <--(in_label),
                            ->(match_label, out_label) := ("matches", 0)
                        ))
                }
            }

            val start_label = rel.label_generator.next("entry")
            val no_match_unravel_label = rel.label_generator.next("exit")
            val match_bypass_label = rel.label_generator.next("match_bypass")

            // Entry block initializes the lists of matchees and matches
            rel.add(block(
                ("matchee", 0) := <--("begin"),
                ->(start_label) := ((("matchee", ()), ()), 0)
            ))

            helper(linearize(pattern), start_label, no_match_unravel_label, match_bypass_label)

            // The exit block constructs the final result. The control flow bit is part of the result.
            rel.add(block(
                ("value", "is_match") := <--(match_bypass_label, no_match_unravel_label),
                ->("end") := (("is_match", "value"), 0)
            ))

            rel.compile()
        }

        def parameters(typ: Syntax.TypeExpression): List[Syntax.TypeExpression] = typ match {
            case Syntax.FunTypeExpression(domain, codomain) => domain :: parameters(codomain)
            case Syntax.BijectionTypeExpression(domain, codomain) => Nil
            case _ => ??? // Unreachable, this is not a valid function type
        }

        def boundVariables(pattern: Syntax.Pattern): Seq[Syntax.VariablePattern] = pattern match {
            case Syntax.TuplePattern(elements) => elements.flatMap(boundVariables)
            case v@Syntax.VariablePattern(name) => Seq(v)
            case Syntax.ConsPattern(head, tail) => Seq(head, tail).flatMap(boundVariables)
            case Syntax.ConstructorPattern(constructor, arguments) => arguments.flatMap(boundVariables)
            case Syntax.NilPattern() => Seq()
            case Syntax.UnitPattern() => Seq()
        }

        def foldWithTerminator(exps: Seq[hssa.Syntax.Expression]): hssa.Syntax.Expression = exps.foldRight(hssa.Syntax.Expression.Unit())(hssa.Syntax.Expression.Pair.apply)

        def foldWithoutTerminator(exps: Seq[hssa.Syntax.Expression]): hssa.Syntax.Expression = exps.reduceRightOption(hssa.Syntax.Expression.Pair.apply).getOrElse(hssa.Syntax.Expression.Unit())

        def translateExpression(block: IncrementalBlockBuilder, exp: Syntax.Pattern, consume: Boolean, direction: Direction, initial_target_variable: Option[hssa.Syntax.Expression.Variable] = None): hssa.Syntax.Expression.Variable = {
            val pattern_f = translatePattern(exp)
            val target_variable = initial_target_variable.getOrElse(block.freshVariable("t"))

            val foldedOperands = foldWithTerminator(boundVariables(exp).reverse.map(v => hssa.Syntax.Expression.Variable(hssa.Syntax.Identifier(mangle_id(v.name.name)))))

            val acc = new ListBuffer[hssa.Syntax.Assignment]

            val input_arg: hssa.Syntax.Expression = if (consume) foldedOperands else {
                val duplicate_var = block.freshVariable("t")
                acc.addOne(
                    duplicate_var :== ("dup", foldedOperands) := (),
                )
                duplicate_var
            }

            acc.addOne(
                target_variable :== (~pattern_f.name, ()) := (0, input_arg)
            )

            block.adds(
                direction match {
                    case Direction.BACKWARDS => hssa.Inversion.Local.invert(acc.toSeq)
                    case _ => acc.toSeq
                }
            )

            target_variable
        }

        /**
         * A function matches to a relation that takes all anciallaries as parameters. They are copied into local variables for matching purposes.
         */
        def translateFunction(function: Syntax.FunctionDefinition): hssa.Syntax.Relation = {
            val params = parameters(function.signature).zipWithIndex.map((p, i) => hssa.Syntax.Expression.Variable(s"p$i"))

            val params_unfolded = params.reduceRightOption(hssa.Syntax.Expression.Pair.apply).getOrElse(hssa.Syntax.Expression.Unit())

            val builder = program_builder.newRelation(mangle(function.name.name), params_unfolded)

            // Each case is a branch on the left-hand pattern and a join of the right-hand pattern.
            // It has exactly one block for the matching case, which receives all bound variables of the left-hand pattern as input
            // and passes all bound variables of the right hand pattern as output.
            // The non-matching case goes to the next case. If there is none left, an error block is added.

            def helper(cases: List[Syntax.Case], in_label: String, out_label: String, index: Int): Unit = {
                cases match {
                    case head :: next =>
                        val match_label_1 = builder.label_generator.next(s"case${index}in")
                        val match_label_2 = builder.label_generator.next(s"case${index}ou")

                        val nomatch_label_1 = builder.label_generator.next(s"branch${index}")
                        val nomatch_label_2 = builder.label_generator.next(s"join${index}")

                        val in_pattern = Syntax.TuplePattern(head.parameter_patterns :+ head.in_pattern)
                        val out_pattern = Syntax.TuplePattern(head.parameter_patterns :+ head.out_pattern)

                        val translated_in_pattern = translatePattern(in_pattern)
                        val translated_out_pattern = translatePattern(out_pattern)

                        // Branch block. Check
                        builder.add(
                            block(
                                ("matchee", 0) := <--(in_label),
                                Seq(
                                    ("is_match", "match_data") := translated_in_pattern.name := "matchee"
                                ),
                                ->(match_label_1, nomatch_label_1) := ("match_data", "is_match")
                            )
                        )

                        val in_variables = boundVariables(in_pattern).reverse.map(v => hssa.Syntax.Expression.Variable(mangle_id(v.name.name))).foldRight(hssa.Syntax.Expression.Unit())(hssa.Syntax.Expression.Pair.apply)
                        val out_variables = boundVariables(out_pattern).reverse.map(v => hssa.Syntax.Expression.Variable(mangle_id(v.name.name))).foldRight(hssa.Syntax.Expression.Unit())(hssa.Syntax.Expression.Pair.apply)

                        val case_body = builder.startBlock((in_variables, 0) := <--(match_label_1))

                        head.body.toSeq.flatMap(_.assigns).foreach(asgn => {

                            // Compute values of ancillaries
                            val par_args: Seq[hssa.Syntax.Expression.Variable] = asgn.parameter_arguments.map(arg => {
                                translateExpression(case_body, arg, false, Direction.FORWARDS)
                            })

                            val par_arg = foldWithoutTerminator(par_args)

                            val input_arg = translateExpression(case_body, asgn.consumed_argument, true, Direction.FORWARDS)

                            val callee: hssa.Syntax.Expression = asgn.direction match {
                                case reversibility.Direction.FORWARDS => mangle(asgn.function.name)
                                case reversibility.Direction.BACKWARDS => ~mangle(asgn.function.name)
                            }

                            val temp_var = case_body.freshVariable("t")

                            case_body.add(
                                temp_var :== (callee, par_arg) := input_arg
                            )

                            // Uncompute values of ancillaries
                            asgn.parameter_arguments.zip(par_args).map((arg, variable) => {
                                translateExpression(case_body, arg, false, Direction.BACKWARDS, Some(variable))
                            })

                            translateExpression(case_body, asgn.pattern, true, Direction.BACKWARDS, Some(temp_var))
                        })

                        case_body.finish2(->(match_label_2) := (out_variables, 0))

                        helper(next, nomatch_label_1, nomatch_label_2, index + 1)

                        // Join block.
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
                        // Error block for match errors.
                        builder.add(block(
                            ((), 1) := <--(in_label),
                            ->(out_label) := ((), 1)
                        ))
                }
            }


            val in_label = builder.label_generator.next(s"branch0")
            val out_label = builder.label_generator.next(s"join0")

            val duplicated_params = params.map(v => hssa.Syntax.Expression.Variable(hssa.Syntax.Identifier(v.name.name + ".copy")))

            def foldDuplicatedParams(right: hssa.Syntax.Expression): hssa.Syntax.Expression = duplicated_params.foldRight(right)(hssa.Syntax.Expression.Pair.apply)

            builder.add(block(
                ("input", 0) := <--("begin"),
                Seq(
                    foldDuplicatedParams(()) :== ("dup", params_unfolded) := ()
                ),
                ->(in_label) := (foldDuplicatedParams("input"), 0)
            ))

            helper(function.cases, in_label, out_label, 1)

            builder.add(block(
                (foldDuplicatedParams("output"), 0) := <--(out_label),
                Seq(
                    () :== (~"dup", params_unfolded) := foldDuplicatedParams(())
                ),
                ->("end") := ("output", 0)
            ))

            builder.compile()
        }

        /**
         * Each constructor maps to a bijection that wraps the arguments in a tuple with the constructor id
         */
        def translateType(typ: Syntax.DataTypeDefinition): Unit = {
            // Data types don't need a translation. Constructors are part of patterns
        }

        lazy val result: hssa.Syntax.Program = {
            runtime_library.foreach(rel => program_builder.add(rel))

            program.defs.foreach {
                case t: Syntax.DataTypeDefinition => translateType(t)
                case f: Syntax.FunctionDefinition => translateFunction(f)
            }

            program_builder.compile()

            hssa.parsing.Parsing(Language.Canon).grammar.parse(
                SourceFile.fromString(hssa.Formatting.format(program_builder.compile()))
            )
        }
    }

    def translateProgram(program: Syntax.Program): hssa.Syntax.Program = ForProgram(program).result
}
