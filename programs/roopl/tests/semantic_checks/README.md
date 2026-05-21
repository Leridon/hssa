# Roopl++ semantic error tests

Each `.rplpp` file triggers **one** semantic error during `roopl.check`.
The first comment line names the error class; the second states the expected message.

Run all:

```bash
sbt 'run load programs/roopl/semantic_errors/01_duplicate_class_name.rplpp, roopl.parse, roopl.check'
```

## Core set (20)

| File | Error |
|------|--------|
| `01_duplicate_class_name` | DuplicateClassName |
| `02_missing_class` | MissingClass |
| `03_duplicate_field_name` | DuplicateFieldName |
| `04_duplicate_method_name` | DuplicateMethodName |
| `05_cyclic_inheritance` | CyclicInheritance |
| `06_missing_main` | MissingMain |
| `07_multiple_mains` | MultipleMains |
| `08_bad_main` | BadMain |
| `09_missing_type` | MissingType |
| `10_bad_typing` | BadTyping |
| `11_bad_assign` | BadAssign |
| `12_impossible_typing` | ImpossibleTyping |
| `13_field_overwrite` | FieldOverwrite |
| `14_bad_method_signature` | BadMethodSignature |
| `15_bad_method_signature_typing` | BadMethodSignatureTyping |
| `16_variable_doesnt_exist` | VariableDoesntExist |
| `17_irreversible_assignment` | IrreversibleAssignment |
| `18_method_doesnt_exist` | MethodDoesntExist |
| `19_argument_doesnt_exist` | ArgumentDoesntExist |
| `20_bad_argument_count` | BadArgumentCount |

## Additional tests

| File | Error |
|------|--------|
| `21_non_unique_args` | NonUniqueArgs |
| `22_field_local_call_arg` | FieldLocalCallArg |

## Unchecked gaps (compile OK, runtime / HSSA)

| File | Issue |
|------|--------|
| `23_self_call_receiver_arg` | `call n::harm(n)` — unchecked aliasing of callee and argument; compiles and runs; not a guaranteed runtime error |
