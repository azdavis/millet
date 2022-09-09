# Versioning

Millet technically follows [SemVer][sem-ver], but the major version is zero, and it probably will be [for a while][zero-ver]. This means Millet makes **absolutely no** stability guarantees between versions.

The versioning system is basically the following:

- The version is split into three numbers, `major.minor.patch`.
- Usually, we increment the "patch" version.
- We increment the "minor" version either:
  - After many "patch" versions.
  - If there's a really "big" change.
- As mentioned, the "major" version is 0.

If the major version ever goes to 1, from then on, we'll actually follow SemVer and provide stability guarantees. Not really sure what this would look like though, since Millet isn't really a "library" or an "API".

[sem-ver]: https://semver.org
[zero-ver]: https://0ver.org
