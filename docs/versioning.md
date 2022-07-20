# Versioning

Currently, Millet **does not** follow [SemVer][sem-ver], and makes **no** stability guarantees between versions.

The versioning system is basically the following:

- The version is split into three numbers, `major.minor.patch`, like SemVer.
- Usually, we increment the "patch" version. Unlike SemVer, we do this even when the changes are breaking, or add functionality.
- We increment the "minor" version either:
  - After many "patch" versions
  - If there's a really "big" change
- The "major" version is 0. It'll probably stay at 0 [for a while][zero-ver].

If the major version ever goes to 1, from then on, we'll probably try to follow SemVer. Not really sure what this would look like though, since Millet isn't really a "library" or an "API".

[sem-ver]: https://semver.org
[zero-ver]: https://0ver.org
