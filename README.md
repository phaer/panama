panama — Control the mpv media player over websockets.
-------------------------------------------------------------------------------
%%VERSION%%

panama is a simple server application, communicating with [`mpv`](mpv) over a
Unix socket and with a client over WebSocket and/or HTTP. It keeps a copy of
the players state in memory and allows developers to easily implement enhancements,
such as advanced play list management.

panama is distributed under the ISC license.

Homepage: https://github.com/phaer/panama  

[mpv]: https://mpv.io

## Installation

panama can be installed with `opam`:

    opam install panama

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
panama`.

[doc]: https://phaer.org/panama/doc

## Sample programs

If you installed panama with `opam` sample programs are located in
the directory `opam var panama:doc`.

In the distribution sample programs and tests are located in the
[`test`](test) directory of the distribution. They can be built and run
with:

    topkg build --tests true && topkg test 
