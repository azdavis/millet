# Configuration

Millet can be configured with a `millet.toml` in the workspace root. It has the following format:

```toml
# version must be exactly 1
version = 1
[workspace]
# set the root cm file. if the workspace has only one root cm file, millet will automatically discover it, but if there is more than one, it must be set here.
root = "foo.cm"
```
