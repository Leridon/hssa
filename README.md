# HSSA Toolkit

This repository contains a toolkit for the Hybrid Static Single Assignment language (HSSA).
It is a reversible intermediate language with optional support for irreversible operations, hence the term "Hybrid".

The toolkit includes an interpreter for the language, along with code analysis and transformation tools.
The project also includes tools for the reversible object oriented language Roopl++, including a compiler to HSSA and an
implementation of the language server protocol.

# Build & Run

The software is implemented in Scala and can be compiled and run with sbt or in an IDE of your choice.

**Run the CLI:**

```sbt run <args>```

The CLI is non-standard, see the documentation below.

**Building an all-in-one jar file**

```sbt assembly```

Packages an über-jar `hssa.jar` with all dependencies included.

# CLI Usage

This software comes with a command line interface that can be used to run most included tools.
The command line interface is a simple concatenative language that can be used to execute and compose a sequence of
commands.
Arguments to the CLI are basically small build-scripts in a custom DSL.
Its syntax has conflicts with most shells (such as bash), so escaping special characters may be necessary.
As a quick reference, examples for common tasks follow.

## Examples

**Run an HSSA program**

```load example.hssa, hssa```

**Autoformat all files of a modular HSSA program**

```load example.hssa, hssa.parse, hssa.formatall, savefiles```

**Translate a Roopl++ program to HSSA and output result to stdout**

```load example.roopl, roopl```

**Translate a Roopl++ program to HSSA and output result to file**

```load example.roopl, roopl, save example_translated.hssa```

**Translate a Roopl++ program and immediately run it**

```load example.roopl, roopl, hssa```

**Translate a Roopl++ program to HSSA, output result to file, and execute it**

```load example.roopl, roopl, tap { save example_translated.hssa }, hssa```

## CLI Syntax

The syntax of commands is given in EBNF below. Whitespace outside of strings is ignored.

```
COMMAND ::= FUN
        ::= FUN ',' COMMAND

FUN     ::= STRING { ARG }

ARG     ::= SIMPLE_ARG
          | NAMED_ARG
      
NAMED_ARG ::= STRING '=' SIMPLE_ARG

SIMPLE_ARG ::= STRING
             | '{' COMMAND '}'
             
STRING      ::= <any uninterrupted sequence of non-whitespace characters except the following: { } , = >
              | '"' <any sequence of characters except "> '"'
```

## Command execution model
Commands are executed from left to right.
At each point in time, there is exactly one active value that the command is applied to.
It can be one of the following types:

| Type            | Description                                                                                                                                             |
|-----------------|---------------------------------------------------------------------------------------------------------------------------------------------------------|
| Unit            | The empty value. This is the initial value.                                                                                                             |
| File            | A reference to exactly one file. Can be either a physical file on disk with a path, or a virtual in-memory file.                                        |
| HSSA            | A parsed HSSA program. Represented as an AST in-memory.                                                                                                 |  
| Modular HSSA    | An HSSA consisting of multiple parsed HSSA programs that were gathered using import statements. Can also be used whereever an HSSA program is expected. | 
| Roopl           | A parsed Roopl++ program as an AST in-memory.                                                                                                           |
| RooplWellformed | A parsed and typechecked Roopl++ program as an in-memory AST with annotated bindings.                                                                   | 
| Sequence [T]    | A generic collection of values. Used for utility functions.                                                                                             |

## Command reference

| Command             | Input Value     | Output Value    | Description                                                                                                                                                                                                                                              |
|---------------------|:----------------|:----------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| load [path]         | Any             | File            | Loads the file from the given path.                                                                                                                                                                                                                      |
| save [path]         | Any             | Unit            | Save the current value as the given path. Formatting depends on the type of value, usually a pretty-printed program.                                                                                                                                     |
| tap [quotation]     | Any             | Unit            | Apply the quoted function to the current value, but keep the current value.                                                                                                                                                                              | 
| foreach [quotation] | Sequence [T]    | Unit            | Requires the current value to be a sequence. Apply the given function to each value.                                                                                                                                                                     |
| dump                | Any             | Any             | Dumps the current value to stdout.                                                                                                                                                                                                                       |
| drop                | Any             | Unit            | Discards the current value.                                                                                                                                                                                                                              |
| savefiles           | Sequence[File]  | Unit            | Saves all files to their respective paths.                                                                                                                                                                                                               |
| hssa.parse          | File            | Modular HSSA    | Requires the current value to be a file. Parses the file as a HSSA program with imports.                                                                                                                                                                 | 
| hssa.check          | HSSA            | HSSA            | Checks the wellformedness of the current HSSA program and throws an error if it is ill-formed.                                                                                                                                                           | 
| hssa.typecheck      | HSSA            | HSSA            | Experimental type checker for the current HSSA program                                                                                                                                                                                                   |                                                
| hssa.exec [main]    | HSSA            | File            | Executes the current HSSA program. The positioned argument name the entry point relation (default "main"). The entry point is invoked with `()` as both the parameter and input value. The result value is made into an in-memory file/free form string. |
| hssa                | File            | File            | Shortcut for `hssa.parse`, `hssa.check` and `hssa.exec` with the default entrypoint.                                                                                                                                                                     |
| hssa.graphs         | HSSA            | Sequence[File]  | Generates various graph representations of the given HSSA program as Dot-graphs. Includes call graphs for the entire program, control flow graphs for procedures, and variable usage graphs for blocks.                                                  |                                                                                                 |
| hssa.optimize.lcp   | HSSA            | HSSA            | Performs local constant propagation on the HSSA program.                                                                                                                                                                                                 |
| hssa.formatall      | Modular HSSA    | Sequence[File]  | Applies autoformatting to all files of the modular HSSA program. CAVE: Currently, autoformatting does not preserve comments.                                                                                                                             |
| roopl.parse         | File            | Roopl           | Parses the given file as a Roopl++ program.                                                                                                                                                                                                              |
| roopl.check         | Roopl           | RooplWellformed | Checks the well-formedness of the given roopl program.                                                                                                                                                                                                   |
| roopl.translate     | RooplWellformed | HSSA            | Compiles the given Roopl program to HSSA. Use `save` to save it to a file or one of the HSSA commands to optimize and/or execute it.                                                                                                                     |
| roopl               | File            | HSSA            | All-in-one shortcut for `roopl.parse`, `roopl.check` and `roopl.translate`. Takes a roopl file and produces an HSSA file.                                                                                                                                |



# HSSA

HSSA is an untyped, reversible intermediate language, with optional support for irreversible operations.
Programs consist of procedures that implement parameterized relations, and each procedure is a flowchart made of blocks
that are connected via labels used in entry and exit statements.
The special labels `begin` and `end` mark the beginning and end of the procedure, the lexical order of the blocks is
irrelevant.
Entries and exits are n-ary, and take/produce a pair of values where the right element is the index of the label.

```
rel fibpair:
     n, 0       :=  begin <-
                      -> L1    =:   (0, 1, n), 0

(a, b, n), l_in :=  L2,L1 <-
      ()        := ~equal 0, a =:       l_in
     l_out      :=  less 0, n  =:        ()
                    -> L4,L3   =: (a, b, n), l_out

(a, b, n.0), 0  :=    L3 <-
      n.1       :=   add -1    =:       n.0
       c        :=    add b    =:        a
                      -> L2    =:  (b, c, n.1), 0

 (a, b, 0), 0   :=    L4 <-
                     -> end    =:    (a, b), 0
```

Further examples can be found in the `examples/hssa` directory.

## Imports

This toolkit also implements an extended language called Modular HSSA that allows importing procedures from other files.
Imports are only allowed at the very top of a program and use the following syntax:

```
import a.b.c        // Resolves to a/b/c.hssa
import ..sibling.a  // Resolves to ../sibling/a.hssa
import ...sibling.a // Resolves to ../../sibling/a.hssa
```

Lexically, import paths are identifiers.
Consequently, they cannot contain whitespace or special characters.
Periods translate to directory separators, and repeated periods resolve to the parent directory `..`.
Imports are recursively resolved, and result in a single HSSA program containing all imported procedures.

# Contributing

Contributions to this project are always welcome, be it bug reports or pull requests.
Easy examples of contributions are additional example programs to enrich the test suite, and additions to the command
line interface.
Larger contributions are also welcome, such as additional optimizations.

# Publications

- Lukas Gail, Uwe Meyer,
  2024, [Connecting Reversible and Classical Computing Through Hybrid SSA](https://www.springerprofessional.de/en/connecting-reversible-and-classical-computing-through-hybrid-ssa/27143942)
- Tristan Schönhals,
  2026, [Design and Implementation of a Compiler for a Reversible Object-Oriented Programming Language](https://publikationsserver.thm.de/xmlui/handle/123456789/479)
- Lukas Gail, (pending), Hybrid Computing (PhD Thesis)

# Contact

Lukas Gail - *lukas.gail@mni.thm.de*