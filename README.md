# Boilerplate JIT

This is a realtime FRP compiler for
[boilerplate](http://josephg.com/boilerplate)! Its fully operational - the
version of boilerplate running live uses this code.

In many ways its a rewrite of the aborted [boilerplate
compiler](https://github.com/josephg/boilerplate-compiler). It has two main
changes:

- Unlike the compiler, boilerplate-jit can handle adjacent shuttles (and
potentially adjacent shuttles). Shuttles are merged just like the
simulator, making it fully
[spec](https://github.com/josephg/boilerplate-sim)-compliant.
- The jit can't actually output javascript. Instead the simulation runs
in-memory. (So actually running a boilerplate sim using this is about 10x
slower than the compiler).
- Everything is just-in-time. If you have a huge boilerplate world, you can
update any small part of the space and have everything update live.

The JIT also supports bundled ribbon cable, although its not an official feature in the web UI.


---

# License

> Standard ISC License

Copyright (c) 2011-2014, Joseph Gentle, Jeremy Apthorp

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

