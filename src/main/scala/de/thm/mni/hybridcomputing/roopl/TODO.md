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
- [-] CLI-usable main
- [+] Improved Error Handling
    - [x] Add erroneous source code to error message output
- [-] Semantic Analysis
    - [x] ~~BindingTree as a helpful datastructure to write wellformedness checks easier~~
    - [-] Rewrite of BindingTree into multiple data structures, which represent the current semantics of the Syntax tree
    - [-] Wellformedness checks
    - [-] Type checking
- [ ] Translation into HSSA
    - ...