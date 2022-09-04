# Configuration

There are two places where Millet can be configured:

- `millet.toml`. This is for project-wide settings.
- VS Code extension settings. This is for user-specific settings.

## `millet.toml`

Millet can be configured with a `millet.toml` in the workspace root. It has the following format:

```toml
version = 1
[workspace]
root = "foo.cm"
[workspace.path-vars]
FOO = { value = "bar" }
QUZ = { path = "lib" }
```

- `version` is the version of the config file. At time of writing, it must be exactly `1`.
- `workspace` is configuration for the workspace.
  - `root` sets the root group file. In the case where there is only one group file in the root, Millet infers it. But if not, it must be set here.
  - `path-vars` is a table for expanding path variables in group files.
    - If the value is a `value`, the value is used unchanged.
    - If it is a `path`, then the value is expanded into a full path relative to the `millet.toml` file.

## VS Code settings

Millet offers the following configuration options via VS Code settings:

### `millet.server.enable`

- Type: `boolean`
- Default: `true`

Enable the language server.

### `millet.server.path`

- Type: `string`
- Default: `""`

Path to the `lang-srv` executable. When set to the empty string `""` (the default), use the path to the one that's pre-built and bundled with the extension.

### `millet.server.hover.token.enable`

- Type: `boolean`
- Default: `true`

Show information about tokens on hover.
