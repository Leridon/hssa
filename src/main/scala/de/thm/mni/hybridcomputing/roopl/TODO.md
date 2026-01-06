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
    - [x] Refactor ScopeTree to be generic so that statements can be swapped with better representations after i.e type-checking is done. This would improve and simplify translation
    - [ ] Ensure arguments to calls are not expression if they are changed inside the method. Or forbid epxression arguments entirely

- [-] Translation into HSSA
    - [x] Translate integer expressions
    - [x] Translate control flow statements
    - [x] Implement memory management in a way sensible to roopl++ as well as fitting into hssa
    - [x] Translate object construction/destruction, vtables, calls etc.
    - [x] ~~Handle reference variables differently than local variables (in a block the variable musn't be persisted into storage, but a class field does). Types of variables:~~
    - [x] Implement variable referencing properly
        - [x] All variables must be stored and referenced because all variables can be copied
        - [x] Handle referencing errors (i.e. nil pointers?)
        - [x] Arrays
        - [] Optimize local variables/fields that are never borrowed anywhere to be static (having a single pointer in hssa) instead of being references that live on the heap
    - [] Properly catch translation errors