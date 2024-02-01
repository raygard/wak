CC = gcc

usage_msg:
	@echo Usage: make target
	@echo Where 'target' is one of: normal asan profiling monolithic musl toy all windows
	@echo all includes normal asan profiling monolithic -- not musl, toy, or windows
	@echo  - Install musl to use 'make musl'
	@echo  - Install msys2 to use 'make windows' -- in Windows

.PHONY: normal asan profiling monolithic all windows clean foo usage_msg

normal: ./exe/wak

asan: ./asan/wak

profiling: ./prof/wak

monolithic: ./mono/wak

mono: monolithic

musl: ./musl/wak

all: normal asan profiling monolithic

toy: ./toybox/awk

windows: ./win/wak.exe

# Order is significant for monolithic source mono.c!
SRC = ./src/common.h ./src/lib.c ./src/common.c ./src/scan.c ./src/compile.c ./src/run.c ./src/main.c

OBJS = ./objects/lib.o ./objects/common.o ./objects/compile.o ./objects/scan.o ./objects/main.o ./objects/run.o

ASANOBJS = ./asanobjects/lib.o ./asanobjects/common.o ./asanobjects/compile.o ./asanobjects/scan.o ./asanobjects/main.o ./asanobjects/run.o

PROFOBJS = ./profobjects/lib.o ./profobjects/common.o ./profobjects/compile.o ./profobjects/scan.o ./profobjects/main.o ./profobjects/run.o

WINOBJS = ./winobjects/lib.obj ./winobjects/common.obj ./winobjects/compile.obj ./winobjects/scan.obj ./winobjects/main.obj ./winobjects/run.obj winobjects/getline.obj

$(OBJS): ./src/common.h

$(WINOBJS): ./src/common.h


CFLAGS = -O3 -funsigned-char  -std=c99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -pedantic -D_POSIX_C_SOURCE=200809L -fsanitize=address -fno-omit-frame-pointer -static-libasan -lrt -g -ggdb
CFLAGS = -O3 -funsigned-char -std=gnu99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -g -ggdb
CFLAGS = -O3 -funsigned-char -std=c99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -g -ggdb
CFLAGS = -O3 -funsigned-char -std=c99 -D_POSIX_C_SOURCE=200809L -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -pedantic -g -ggdb -fsanitize=address
CFLAGS = -O3 -funsigned-char -std=c99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes
CFLAGS = -O3 -funsigned-char -std=c99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -pedantic -D_POSIX_C_SOURCE=200809L -g -ggdb
CFLAGS = -O3 -funsigned-char -std=c99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -D_POSIX_C_SOURCE=200809L -g -ggdb

#LDFLAGS = -flto -lm -llibregex -Xlinker -Map=./objects/link.map
#LDFLAGS = -flto -lm -Xlinker -Map=./objects/link.map -fsanitize=address
#LDFLAGS = -flto -lm -Xlinker -Map=./objects/link.map

./exe/wak: LDFLAGS = -flto -lm -Xlinker -Map=./objects/link.map
./exe/wak: $(OBJS)
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

./objects/%.o: ./src/%.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@

./asan/wak: LDFLAGS = -flto -lm -Xlinker -Map=./asanobjects/link.map -fsanitize=address
./asan/wak: $(ASANOBJS)
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

./asanobjects/%.o: ./src/%.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -fsanitize=address -fno-omit-frame-pointer -static-libasan -c $< -o $@


./prof/wak: LDFLAGS = -flto -lm -Xlinker -Map=./profobjects/link.map -pg
./prof/wak: $(PROFOBJS)
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

#./profobjects/%.o: CFLAGS = -O3 -funsigned-char -std=c99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -g -ggdb -pg -fprofile-arcs -ftest-coverage
#./profobjects/%.o: CFLAGS = -O3 -funsigned-char -std=c99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -g -ggdb -pg
#./profobjects/%.o: CFLAGS = $(CFLAGS) -pg -fprofile-arcs -ftest-coverage
./profobjects/%.o: ./src/%.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -pg -c $< -o $@


./mono/wak: LDFLAGS = -flto -lm -Xlinker -Map=./monoobjects/link.map
./mono/wak: ./monoobjects/mono.o
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

./monoobjects/%.o: ./monosrc/%.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@

./monosrc/mono.c: $(SRC)
	@mkdir -p $(@D)
	awk -f ./scripts/make_mono.awk $(SRC) > $@


./musl/wak: CC = /usr/local/musl/bin/musl-gcc
./musl/wak: LDFLAGS = -static -s
./musl/wak: ./muslobjects/mono.o
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

#CFLAGS = -Os -funsigned-char -std=c99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -pedantic -D_POSIX_C_SOURCE=200809L
#LDFLAGS = -static -s
./muslobjects/%.o: CC = /usr/local/musl/bin/musl-gcc
./muslobjects/%.o: ./monosrc/%.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -Os -c $< -o $@

./toybox/awk: ./monosrc/mono.c
	@mkdir -p $(@D)
	awk -f ./scripts/make_toybox_awk.awk ./monosrc/mono.c > $@.c

./win/wak.exe: LDFLAGS = -flto -lm -llibregex -Xlinker -Map=./winobjects/link.map
./win/wak.exe: $(WINOBJS)
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

./winobjects/%.obj: CFLAGS = -O1 -funsigned-char -std=c99 -Wall -Wextra -W -Wpointer-arith -Wstrict-prototypes -pedantic -D_POSIX_C_SOURCE=200809L
./winobjects/%.obj: ./src/%.c
	@-mkdir $(@D)
	$(CC) $(CFLAGS) -c $< -o $@
./winobjects/%.obj: ./winsrc/%.c
	@-mkdir $(@D)
	$(CC) $(CFLAGS) -c $< -o $@

clean :
	-rm ./exe/*
	-rm ./asan/*
	-rm ./prof/*
	-rm ./mono/*
	-rm ./musl/*
	-rm ./win/*
	-rm ./objects/* ./asanobjects/* ./profobjects/* ./monoobjects/* ./muslobjects/* ./winobjects/*

foo:
	@echo cpp: $(SRC)
	@echo objs: $(OBJS)
	@echo profobjs: $(PROFOBJS)
	@echo winobjs: $(WINOBJS)
	@echo exe: wak
	@echo srcdir: ./src
	@echo objdir: ./objects
	@echo profobjdir: ./profobjects
	@echo exedir: ./exe
	@echo asandir: ./asan
	@echo profdir: ./prof

#######################
