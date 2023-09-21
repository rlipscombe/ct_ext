# ct_report

Pretty progress reporting and summary for Erlang's Common Test.

This package provides the following hook:

- `ct_summary` -- print a list of the succeeded and failed tests after the run has completed.

I plan to add other hooks in future.

## Usage

This project only supports `erlang.mk` or directly using `ct_run`, since I'm not sure that `rebar3` needs it.

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

## Demonstration

Run `make ct` in this project.

## Debugging problems with the hook

1. Common Test outputs the path of the `all_runs.html` file. Open that file.
2. Click on the most-recent test run.
3. Click on the "Common Test Framework Log" button.
4. Hook errors are usually in the "Progress Log" part of the page.
