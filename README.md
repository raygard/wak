## wak

`wak` is a fairly compact implementation of the awk programming language.

It is mostly compatible with POSIX awk, Busybox awk, mawk, and Brian Kernighan's One True Awk (the original Unix awk), and with gawk except for most gawk extensions to the original awk language.

This repo contains the modular version of the source, the one-file "monolithic" version of the source, and the "toybox" version of the source (intended for use with Rob Landley's toybox project).

Running `make` will show
```
Usage: make target
Where target is one of: normal asan profiling monolithic musl toy all windows
all includes normal asan profiling monolithic -- not musl, toy, or windows
- Install musl to use make musl
- Install msys2 to use make windows -- in Windows
```

You can also just compile the `mono.c` version in Linux:
```
gcc -Os -funsigned-char -std=c99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -pedantic  -D_POSIX_C_SOURCE=200809L  mono.c -static -s -o wak -lm
```
or
```
gcc -Os -funsigned-char -std=gnu99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -pedantic  mono.c -static -s -o wak -lm
```
or, if you have `musl` installed, you can get much smaller binaries with:
```
musl-gcc -Os -funsigned-char -std=c99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -pedantic -D_POSIX_C_SOURCE=200809L mono.c -static -s -o wak
```
or
```
musl-gcc -Os -funsigned-char -std=gnu99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -pedantic mono.c -static -s -o wak
```

Also, you can compile this with TCC (Tiny C Compiler, aka tcc) or run it as a script! Install the version maintained at https://repo.or.cz/w/tinycc.git, or the unofficial mirror which is at https://github.com/TinyCC/tinycc, and put this line at the top of the `mono.c` file:
```
#!/usr/local/bin/tcc -run -funsigned-char
```
(Note it will not run correctly without -funsigned-char.) Thank you @davidar for this tip!

### Compatibility

`wak` tries to be compatible with [POSIX.1-2024](https://pubs.opengroup.org/onlinepubs/9799919799/utilities/awk.html), the major exception being that `wak` does not honor the various locale-related parts of the spec. Instead, it follows Rob Landley's toybox approach of trying to be as "UTF-8 safe" as reasonably possible, but do not attempt to be locale-conformant beyond that.
Other departures are made to better match actual implementations rather than the POSIX spec exactly. For example, `wak` accepts regular expressions for the record separator RS, as do other common implementations. Another example: POSIX says "Field variables shall have the uninitialized value when created from $0 using FS and the variable does not contain any characters." `wak` creates field variables as strings, never the uninitialized value, just as other awks do.

In general, `wak` follows POSIX except where it conflicts with traditional and common practice in other implementations.

Since the POSIX update was released only recently, I am still reviewing it to see what changes `wak` needs to make it more conformant.

### Bugs

I'm sure there are plenty of bugs.
Brian Kernighan maintained the original awk for almost 40 years and made over 200 fixes from 1987 to 2023, so I expect my version to have bugs for quite a while also.

Please report bugs to raygard at gmail.com.
