# Configuration

Millet can be configured with a `millet.toml` in the workspace root. It has the following format:

```toml
version = 1
[workspace]
root = "foo.cm"
[workspace.path-vars]
FOO = "bar"
```

- `version` is the version of the config file. At time of writing, it must be exactly `1`.
- `workspace` is configuration for the workspace.
  - `root` sets the root group file. In the case where there is only one group file in the root, Millet infers it. But if not, it must be set here.
  - `path-vars` is a table for expanding path variables in group files.
