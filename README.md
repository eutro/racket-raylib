# racket-raylib

[![CI](https://github.com/eutro/racket-raylib/actions/workflows/ci.yml/badge.svg)](https://github.com/eutro/racket-raylib/actions/workflows/ci.yml)
[![Raylib](https://img.shields.io/badge/raylib-4.0.0-green)](https://github.com/raysan5/raylib/releases/tag/4.0.0)
[![License](https://img.shields.io/badge/license-MIT%2FApache--2.0-blue)](#license)

Semi-automatically generated
[Raylib](https://github.com/raysan5/raylib) bindings for Racket.

`raylib/2d/unsafe` (re-)exports all of `raylib.h` which is relevant
for 2D rendering and games, and is the primary target for this software.
For more details on excluded functions, consult the documentation.

_All_ functions and types from
[`raylib.h`](https://github.com/raysan5/raylib/blob/master/src/raylib.h)
have definitions in `raylib/generated/unsafe`. Most of these should
work, but there is at least one function for which this is impossible.

Functions in
[`raymath.h`](https://github.com/raysan5/raylib/blob/master/src/raymath.h)
have been converted to Racket manually, so they may shift out of line
with the version linked into Raylib. They were written to match those
in the Raylib 4.0.0 release.

Racket examples can be found in the [examples](examples) directory, C
examples can be found in [Raylib's
examples](https://github.com/raysan5/raylib/tree/master/examples)
directory.

## Installation

To install this package:

```shell
git clone git@github.com:eutro/racket-raylib.git
cd racket-raylib
raco pkg install
```

If you're on a common platform, then it should be enough to just
install this package without installing Raylib separately. However, if
you are not so lucky, you may need to [build and install Raylib
globally](https://github.com/raysan5/raylib#build-and-installation).

## License

This software is distributed under the MIT license and the Apache
version 2.0 license, at your option. See [LICENSE-MIT](LICENSE-MIT)
and [LICENSE-APACHE](LICENSE-APACHE) for details.
