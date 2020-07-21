# Testing

A test is a directory directly inside the directory `tests`.

If the test contains a file `run.sh`, then when that file is run with
`sh -eu run.sh`, it must exit 0.

Else, if the test contains a file `ast.sml`, then when the Millet CLI is run to
output AST for that file, it must exit 0 and produce the output in `out.txt`.

Else, if the test contains a file `ok.sml`, then when the Millet CLI is run with
that file on quiet mode, it must exit 0 and produce no output.

Else, if the test contains a file `err.sml`, then when the Millet CLI is run
with that file, it must exit with a non-zero exit code and produce the output in
`out.txt`.

Run `bin/run-test tests/<name>` to run a test. The test runner exports two
environment variables available for use in any `run.sh`:

- `NO_COLOR=1`, to turn off colored output when running the Millet CLI.
- `MILLET`, which is an absolute path to the Millet CLI binary.

After you first create a test, or if you update a test, you may want to generate
an appropriate `out.txt` file. Use `bin/run-test -g tests/<name>` to do that.
