# ROOPL++ compiled to HSSA, TODOs:
- [x] Done
- [-] In progress
- [ ] TBD

---

- [x] Syntax
- [x] Scanner
- [x] Parser
    - [x] Full implementation of Syntax
    - [x] Testing
- [-] Formatting
    - [x] Print Syntax as SourceCode
    - [ ] Add newlines where sensible
    - [ ] Don't lose comments when formatting (requires comments in Syntax)
- [x] CLI-usable main
- [x] Improved Error Handling
    - [x] Add erroneous source code to error message output
- [x] Semantic Analysis
    - [x] ~~BindingTree as a helpful datastructure to write wellformedness checks easier~~
    - [x] Rewrite of BindingTree into multiple data structures, which represent the current semantics of the Syntax tree
    - [x] Wellformedness checks
    - [x] Type checking
    - [ ] Possibly add more and cleverer semantic checks in the future
- [-] Translation into HSSA
    - [ ] Translate integer expressions
    - [ ] Translate control flow statements
    - [ ] Implement memory management in a way sensible to roopl++ as well as fitting into hssa
    - [ ] Translate object construction/destruction, vtables, calls etc.