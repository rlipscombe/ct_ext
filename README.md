# ct_ext

Various extensions for Erlang's Common Test.

This package provides the following hook:

- `ct_ext_summary` -- pretty print a list of the succeeded and failed tests after the run has completed.
- `ct_ext_ensure_started` -- automatically start an application before the test run begins.

I plan to add other extensions in future.

## Usage

This project only supports `erlang.mk` or directly using `ct_run`, since I'm not sure that `rebar3` needs it.

### Using `erlang.mk`

```makefile
TEST_DEPS += ct_ext

dep_ct_ext = git https://github.com/rlipscombe/ct_ext

# -ct_hooks should come near the beginning.
CT_HOOKS = ct_ext_summary
CT_HOOKS += and ct_ext_ensure_started $(APPLICATION)
CT_OPTS = -ct_hooks $(CT_HOOKS) $(CT_OPTS)
```

```sh
make ct
```

## Demonstration

Run `make ct` in this project.

## Contributing

### Debugging problems with the hook

1. Common Test outputs the path of the `all_runs.html` file. Open that file.
2. Click on the most-recent test run.
3. Click on the "Common Test Framework Log" button.
4. Hook errors are usually in the "Progress Log" part of the page.

If there's nothing there, try the following instead:

1. Common Test outputs the path of the `all_runs.html` file. Open that file.
2. Click on the most-recent test run.
3. Look for the failing suite. Click on that.
4. Look for the failing test. Click on that.
5. There's usually a big red banner and a chunk of text. Your error is in there somewhere.

### Using the `ct_ext_debug` hook

```makefile
CT_HOOKS += and ct_ext_debug
```

This will write all the callbacks to the console, so you can figure out which functions your hook is missing.
