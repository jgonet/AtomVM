## Test organization

Test names should belong to one of following categories:

- `vm` – VM behaviours, e.g. specific opcodes, VM mechanisms. Example: `vm_guards.erl` that tests all functions that can be used in guards, what happens if these raise, etc.
- `bif` and `nif` – More detailed testing for all implemented NIFs and BIFs. E.g. `is_pid` can be tested as guard in `vm_guards` but `bif_is_pid` should test if it returns correct response for dead PID, live PID, local PIDs, etc. Commonly, tests will be aggregated, e.g. `nif_ets` for all ETS functions. If some test becomes too big to manage it can be split to logical chunks, e.g. `nif_ets_select`.
- `gc` – tests that tests some patterns for garbage collectors. Example: `make_garbageX` could be `gc_ast_tree`, `gc_list_concat`, etc. If possible, they could be aggregated and tested in scenarios with multiple processes, sharing terms, etc.
- `example` (?) – tests that are testing useful interactions that doesn't fall into other categories. Need to have description what's happening to not accidentally break what's the test is testing.

Some tests should be deleted. There's 21 implementations of factorial. Would be nice to factor them into functions that are more descriptive in function, e.g. `tail_calls(N)`, `tail_calls_raise_after(N)`, `fun_no_gc(X)`, etc. I suspect there's a lot of tests with overlaping test area. We don't need many tests that check addition, it will suffice to have one test case named `example_simple` and one `bif_arith`.

Examples of modules:

```
vm_send_receive
vm_match
vm_guards
vm_integer_limits
vm_many_functions
bif_erlang_is_record
nif_binary
gc_deep_nesting
```

## Structure

Tests should follow basic structure:

```
start() ->
    ok = test_behavior1(),
    ok = test_behavior2(),
    % ...
    0.
```

`test_behaviorX` functions shouldn't nest (e.g. `test_behaviorXUnderCond`), think of it as `describe` in Elixir.

## Helpers

Over time, we're starting to see some emergent patterns when testing. We could move some common helpers to `utils.hrl` which will have macros. Some useful examples: `?fail_badarg(Fun)`, `?fail(Type, Fun)`, `?heap_usage(Term)`, `?reverse(List)`, `?list_eq(List1, List2)`, etc.
Some will mirror functions defined in `erlang` module but we can't use modules with Erlang code here.
