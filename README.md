# HSSA Toolkit

This repository contains a toolkit for the Hybrid Static Single Assignment language (HSSA).
It is a reversible intermediate language with optional support for irreversible operations, hence the term "Hybrid".

The toolkit includes an interpreter for the language, along with code analysis and transformation tools.
The project also includes tools for the reversible object oriented language Roopl++, including a compiler to HSSA and an
implementation of the language server protocol.

# Usage

The software is written in Scala and can be compiled and run with sbt or in an IDE of your choice.
It includes a command line interface for running most of the included tools

The command line interface is a simple concatenative language that can be used to execute and compose a sequence of
commands.
It follows the following syntax:

```
COMMAND ::= FUN
        ::= FUN ";" COMMAND

FUN     ::= string { ARG }

ARG     ::= SIMPLE_ARG
          | NAMED_ARG
      
NAMED_ARG ::= string "=" SIMPLE_ARG

SIMPLE_ARG ::= string
             | "{" COMMAND "}"
```

Commands are executed from left to right.
At each point in time, there is exactly one active value that the command is applied to.
It can be one of the following types:

| Type            | Description                                                                                                                                             |
|-----------------|---------------------------------------------------------------------------------------------------------------------------------------------------------|
| Unit            | The empty value. This is the initial value.                                                                                                             |
| File            | A reference to exactly one file. Can be either a physical file on disc with a path, or a virtual in-memory file.                                        |
| HSSA program    | A parsed HSSA program. Represented as an AST in-memory.                                                                                                 |  
| Modular HSSA    | An HSSA consisting of multiple parsed HSSA programs that were gathered using import statements. Can also be used whereever an HSSA program is expected. | 
| Roopl           | A parsed Roopl++ program as an AST in-memory.                                                                                                           |
| RooplWellformed | A parsed and typechecked Roopl++ program as an in-memory AST with annotated bindings.                                                                   | 
| Sequence [T]    | A generic collection of values. Used for utility functions.                                                                                             |

The following commands are available:

| Command             | Input           | Output          | Description                                                                                                                                                                                             |
|---------------------|:----------------|:----------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| load [path]         | Any             | File            | Loads the file from the given path.                                                                                                                                                                     |
| save [path]         | Any             | Unit            | Save the current value as the given path. Formatting depends on the type of value, usually a pretty-printed program.                                                                                    |
| tap [quotation]     | Any             | Unit            | Apply the quoted function to the current value, but keep the current value.                                                                                                                             | 
| foreach [quotation] | Sequence [T]    | Unit            | Requires the current value to be a sequence. Apply the given function to each value.                                                                                                                    |
| hssa.parse          | File            | HSSA            | Requires the current value to be a file. Parses the file as a HSSA program with imports.                                                                                                                | 
| hssa.check          | HSSA            | HSSA            | Checks the wellformedness of the current HSSA program and throws an error if it is ill-formed.                                                                                                          | 
| hssa.typecheck      | HSSa            | HSSA            | Experimental type checker for the current HSSA program                                                                                                                                                  |                                                
| hssa.exec [main]    | HSSA            | File            | Executes the current HSSA program. The positioned argument name the entry point relation (default "main"). The entry point is invoked with `()` as both the parameter and input value.                  |
| hssa                | File            | File            | Shortcut for `hssa.parse`, `hssa.check` and `hssa.exec` with the default entrypoint.                                                                                                                    |
| hssa.graphs         | HSSA            | Sequence[File]  | Generates various graph representations of the given HSSA program as Dot-graphs. Includes call graphs for the entire program, control flow graphs for procedures, and variable usage graphs for blocks. |                                                                                                 |
| hssa.optimize.lcp   | HSSA            | HSSA            | Performs local constant propagation on the HSSA program.                                                                                                                                                |
| roopl.parse         | File            | Roopl           | Parses the given file as a Roopl++ program.                                                                                                                                                             |
| roopl.check         | Roopl           | RooplWellformed | Checks the well-formedness of the given roopl program.                                                                                                                                                  |
| roopl.translate     | RooplWellformed | HSSA            | Compiles the given Roopl program to HSSA. Use `save` to save it to a file or one of the HSSA commands to optimize and/or execute it.                                                                    |

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