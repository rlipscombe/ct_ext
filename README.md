# ct_report

Pretty progress reporting and summary for Erlang's Common Test.

This package provides the following hook:

- `ct_summary` -- print a list of the succeeded and failed tests after the run has completed.

I plan to add other hooks in future.

## Usage

### Using `erlang.mk`

```makefile
TEST_DEPS += ct_report

dep_ct_report = git https://github.com/rlipscombe/ct_report

# -ct_hooks should come near the beginning.
CT_HOOKS = ct_summary
CT_OPTS = -ct_hooks $(CT_HOOKS) $(CT_OPTS)
```

```sh
make ct
```
