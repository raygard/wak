## wak

`wak` is a fairly compact implementation of the awk programming language.

It is mostly compatible with POSIX awk, Busybox awk, mawk, and Brian Kernighan's One True Awk (the original Unix awk, aka nawk), and with gawk except for most gawk extensions to the original awk language.

This repo contains the modular version of the source, the one-file "monolithic" version of the source, and the "toybox" version of the source (intended for use with Rob Landley's toybox project).

The code is C99, but one feature, anonymous unions, used in the `struct zvalue` type, is not in C99. It is supported in C99 mode in both gcc and clang. Compiling with `gcc -pedantic` will generate warnings. If this is an issue for anyone, please let me know and I will modify the code to avoid it. It can be avoided easily but will clutter the code a little.

#### Acknowledgement

Many thanks to Prof. Nelson H. F. Beebe (Univ. of Utah) for advice on improving the program and makefile. He spent considerable time testing `wak` on different systems and writing suggestions to make `wak` better.

### Building, testing, installing

- Running `make` or `make all` or `make wak` will build the `wak` binary executable from the modular source files.
- `make check` or `make test` will run over 200 validation tests in a few seconds.
- `make install` will copy `wak` to `/usr/local/bin` by default. (May need `sudo make install`)
- `make help` will display all (?) make options.
- You may set `DESTDIR=` and/or `prefix=` as `make` args.
- An improved `configure` script is included. You may use options `CC=`, `CFLAGS=`, `--prefix=` to control compilation and installation. This is a minimal hand-written script; it does not probe for dependencies.

With `msys2` installed under Windows, `make win` will build a `wak.exe` for Windows.

These files compile on my machine with `gcc 13.2.0` and `clang 19.1.0`. The build defaults to using gcc.

You can also just compile the `onefile/wak.c` version in Linux:
```
gcc -Os -funsigned-char -std=c99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -pedantic  -D_POSIX_C_SOURCE=200809L  wak.c -static -s -o wak -lm
```
or
```
gcc -Os -funsigned-char -std=gnu99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -pedantic  wak.c -static -s -o wak -lm
```
or, if you have `musl` installed, you can get much smaller binaries with:
```
musl-gcc -Os -funsigned-char -std=c99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -pedantic -D_POSIX_C_SOURCE=200809L wak.c -static -s -o wak
```
or
```
musl-gcc -Os -funsigned-char -std=gnu99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -pedantic wak.c -static -s -o wak
```

Also, you can compile this with TCC (Tiny C Compiler, aka tcc) or run it as a script! Install the version maintained at https://repo.or.cz/w/tinycc.git, or the unofficial mirror which is at https://github.com/TinyCC/tinycc, and put this line at the top of the `wak.c` file:
```
#!/usr/local/bin/tcc -run -funsigned-char
```
(Note it will not run correctly without `-funsigned-char`.) Thank you @davidar for this tip!

#### A note on the test suite

The test files are currently all taken verbatim from the gawk 5.3.1 distribution, and are under GPL2. See `COPYING` in the `test` directory. (Not all gawk tests are included.) I intend to include more tests from other sources at some point.

### Compatibility

`wak` tries to be compatible with [POSIX.1-2024](https://pubs.opengroup.org/onlinepubs/9799919799/utilities/awk.html), the major exception being that `wak` does not honor the various locale-related parts of the spec. Instead, it follows Rob Landley's toybox approach of trying to be as "UTF-8 safe" as reasonably possible, but does not attempt to be locale-conformant beyond that.

Other departures are made to better match actual implementations rather than the POSIX spec exactly. For example, `wak` accepts regular expressions for the record separator RS, as do other common implementations. Another example: POSIX says "Field variables shall have the uninitialized value when created from $0 using FS and the variable does not contain any characters." `wak` creates field variables as strings, never the uninitialized value, just as other awks do.

In general, `wak` follows POSIX except where it conflicts with traditional and common practice in other implementations.

Since the POSIX update was released only recently, I am still reviewing it to see what changes `wak` needs to make it more conformant.

### Bugs

I'm sure there are plenty of bugs.
Brian Kernighan maintained the original awk for almost 40 years and made over 200 fixes from 1987 to 2023, so I expect my version to have bugs for quite a while also.

Please report bugs to raygard at gmail.com.
