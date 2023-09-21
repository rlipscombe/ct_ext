# ct_report

Pretty progress reporting and summary for Erlang's Common Test.

## Usage

### Using `erlang.mk`

```makefile
TEST_DEPS += ct_report

dep_ct_report = git https://github.com/rlipscombe/ct_report

# -ct_hooks should come near the beginning.
CT_OPTS = -ct_hooks $(CT_HOOKS) $(CT_OPTS)
```

```sh
make ct
```
