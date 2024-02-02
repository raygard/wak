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

### Bugs

I'm sure there are plenty of bugs.
Brian Kernighan maintained the original awk for almost 40 years and made over 200 fixes from 1987 to 2023, so I expect my version to have bugs for quite a while also.

Please report bugs to raygard at gmail.com.
