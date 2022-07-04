# Configuration

Millet can be configured with a `millet.toml` in the workspace root. It has the following format:

```toml
version = 1
[workspace]
root = "foo.cm"
```

- `version` is the version of the config file. At time of writing, it must be exactly `1`.
- `workspace` is configuration for the workspace.
  - `root` sets the root CM file. In the case where there is only one CM file in the root, Millet infers it. But if not, it must be set here.
