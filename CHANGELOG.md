# Changelog

## 2024-10-08 
- Fix bug assigning NF=0; add toybox test for it
- Fix bug assigning NF negative
- Add long options --help, --version

## 2024-10-05 
- Mute clang warnings

## 2024-10-01 
- Add tests for awk split() on empty regex (toybox)
- Fix out-of-bounds memory access in splitter()

## 2024-09-30 
- Rewrite record-reading routines

## 2024-09-07 
- Add tests (toybox)
- Fix field split bug when RS="" and FS is one char

## 2024-08-29
- Add CHANGELOG.md, updated README.md
- Mod to permit RS="()" to work as it does in gawk, nawk, goawk

## 2024-08-25
- Clean up compile code

## 2024-08-24
- Fix field splitting and split() bugs

## 2024-08-10
- Make srand() compliant with POSIX.2024 spec.

## 2024-08-07
- Add test titled "getline var numeric string bug fixed 20240514" to test the "getline var" bug fixed 20240514.
- Fix implementation of nextfile statement. Was picking up "leftover" records in buffer from previous file.
- Add test for nextfile fix
- Add awk_exit() function to allow toybox to clean up on exit -- fixed ASAN mem leak complaint
- Fix "interactive" input behavior (see comments in issue #7)

## 2024-05-14
- Fix 'getline var' bug

    'getline var' was not setting var as numeric string when needed. Found when running https://github.com/patsie75/awk-mandelbrot/raw/master/mandelbrot.awk
```
    echo 5.0 | wak 'BEGIN { getline var; print (var < 10.0) }'
```
should print 1. Older versions of wak printed 0.

## 2024-05-07
- Update awk.test

    Fix 64 bit rshift test; remove trailing spaces; a few other tweaks

## 2024-05-05
- clean up pass on run.c

    Inline set_string(); rename val_to_str to to_str and val_to_num to to_num; replace str_to_num with atof; other cleanups

## 2024-05-03
- Fix printf "%c" bug; fsprintf() cleanup; add tests
- printf "%c", "ú" printed wrong UTF-8; fixed. Still have printf/sprintf() bugs.
- Added toybox awk tests.

## 2024-04-17
- Improve printf/sprintf() format handling

    Research help from @oliverkwebb

## 2024-04-13
- Implement \u escape for Unicode in scan.c

    Authored by Oliver Webb

- Implement -b in bytesinutf8() (fixes substr())

    Also fixes mono.c that didn't get updated in prev. commit

    Credit to Oliver Webb

## 2024-04-12
- Group Rob Landley's code together; add his copyright

    Need a few functions from Landley's toybox when making standalone wak versions

- Mod UTF-8 support to obviate most dynamic allocation

    Mostly need to count codepoints in UTF-8 strings, so no need to allocate / free space for converted wide chars etc. Also allow for expansion in toupper() / tolower()

- Add -b option (use bytes, not characters)

    Authored by @oliverkwebb

- Initial UTF-8 support

    Research and initial implementation by @oliverkwebb

- Merge rx_escape_str() into escape_str(), remove new_zstring_cap()

    Cleanup to remove/combine code

    Authored by @oliverkwebb

## 2024-04-06
- Move math builtin code into main interp loop

    Inlined the math builtins into the main interpreter loop (thanks Oliver Webb), updated toybox test file with bitwise ops tests

- Add bitwise operations

    From Oliver Webb, add some bitwise functions from gawk.

## 2024-03-31
- Handle array deletions better

    Hash insert to first vacant *or* first marked-deleted slot

- Move setlocale(LC_NUMERIC, "") call

    Do not do setlocale() on every conversion; just once at startup

- Fix WEXITSTATUS() usage

## 2024-03-27
- Use random() / srandom() for toybox

- Add toybox awk test file

    Included original from toybox mailing list 2015: http://lists.landley.net/pipermail/toybox-landley.net/2015-March/015201.html. Had a couple bad tests.

- Make file_or_pipe be 1/0 not 'f'/'p'

- Tweak run.c format; move mathfunc[] into math_builtin(); rename time_ms() -> millinow()

- Replace rx_compile(rx, pat) with xregcomp(rx, pat, REG_EXTENDED)

- Remove unneeded inits of TT.scs elements

    TT.scs points to a struct already inited to zero where it's allocated in awk().

- Update Makefile: Use -f to ignore missing files on rm

- Change exprn(n) to expr(n); expr() to expr(0)

## 2024-03-26
- Remove debug code; replace envp with environ

## 2024-02-22
- Allow nul bytes in data files (not in source code yet). Still cannot have nul in multiline record (RS="") data. Work in progress.

- Remove declarations for dev-only debugging build

    Minor code cleanup of some code needed only for dev debugging build (not in repo).

- Replace top of stack index with pointer

    Profiling showed push_val() taking a lot of time. These changes make it several percent faster on many tests. Now test for stack getting close to overflow only at function call. Still possible if a source construct uses an absurd number of stack slots. Possible to test for that in compile phase but not yet worth the effort (and code space).

## 2024-02-15
- Fix memory leak in assign_global()

    awk -v n=x 'BEGIN{f(n)};function f(n){}' caused a mem leak; see comment in assign_global(). Also tweak push_val(), zlist_append_zvalue(); change an exit(42) to exit(2).

## 2024-02-03
- Fix previous fix for comma flag in printf

    Fixed (again) the regex for printf format. "-" needs to go first or else be escaped (to not be a range indicator). Moved "'" to end of "flags" bracket expression.

- Match toybox error_exit() (puts newline at end); fix up error messages

- Allow "'" in printf format for comma thousands separators; make ENVIRON init not modify program environment

## 2024-02-02
- Update README.md

## 2024-02-01
- Mod Makefile to keep mono.c and toybox/awk.c after 'make clean'; add those sources to repo

- Update scripts to make cleaner mono.c and toybox awk.c files with copyright etc.

- For toybox awk, change -r opt to -c, fix up usage message

- Minor code cleanup: Remove unref'd opt_print_source (common.h) and a few commented-out lines (compile.c)

- Update run.c: Remove unneeded force_maybemap_to_scalar() calls

- Update main.c: Drop bogus -p option; change -r opt to -c (compile only); avoid warning on ?: operator missing middle term

## 2024-01-29
- bug fixes and cleanup: Fixed some problems with maybemap objects; allow newlines before 'while' in 'do{}while()'; some code cleanup

## 2024-01-27
- Close files at end of run, except stdin stdout stderr

- Cleanup, incl upcasing #define's

    toybox preferred style is uppercase #define names

- Prelim handling of regex RS

    RS can be a regex in most awks, though not in current POSIX spec. This may not be the best way but seems to work OK.

- Simplify print, printf parsing

## 2024-01-08
- Minor code cleanup: Fixed a few comments; tweaked code in print_stmt() for compiling output modes.

- Fix setup_lvalue() bug

    setup_lvalue() can get a scalar in an array  context and was leaving stack in a bad state. Fix to issue fatal diagnostic. Also tweak to keep ip always pointing to allocated space.

- Clean up messages, message functions and exit code

    Try to conform better to toybox conventions; merge some message code

## 2024-01-03
- Change option arg handling

    Changed arrays of -f and -v options into linked lists (arg_list) as used in toybox

- Update file handling

    Change zfile array to linked list to reduce global space and remove limit on open files. Also format tweaks and minor fix to Makefile

## 2023-12-20
- Code format cleanup: Remove extra spaces, tweak comments, rename an enum.

- Reformat lbp_table[], add a few comments, rename a couple of enums.

- Updated top and bottom toybox_awk_parts

    Mod to move globals into GLOBALS, use toybuf for pbuf (number formatting buffer), remove 'p' option flag (unused in toybox awk).

- Update make_mono.awk

    Fixed up to accommodate 0BSD license line.

## 2023-12-19
- Clean up scan.c

    Move tokstr and prevtok into global TT; removed scan_div_ok() and scan_regex_ok(). Change MAX() to maxof() (for toybox).

- Move pbuf[] (buffer for formatting number to string) into global TT struct. Set up in main(), uses toybuf in toybox.

- Moved global variables into a struct named TT to conform with toybox design.
- Merge: 25706ea 0bfd5db

- Merge branch 'main' of https://github.com/raygard/wak

- typedef removal

    Toybox owner does not like typedefs.

## 2023-12-15
- Update LICENSE

    Word-wrapped now.

- Create LICENSE (0BSD)

- Create README.md - Initial commit.

- Add makefile and associated files

    Initial commit. Makefile and files needed to make monolithic (one file) and toybox versions.

- Skip missing ARGV elements in nextfilearg()

    Was failing delargv.awk test. ARGV elements can be deleted, but should then be skipped when going to next file.

- Set scalar or map (array) type on special variables, to catch type errors in compile phase.

## 2023-12-10
- Code style cleanup pass

## 2023-12-04
- Fix memory leak and type bug in setup_lvalue() (run.c)

## 2023-12-02
- Rewrite printf handling in run.c

- Get compile error count as exit code

- Setup locale in main.c (a la toybox)

## 2023-11-07
- Clean up compile.c -- error messages, function names

## 2023-11-02
- Style -- remove space after '!'

- Improve random number gen default seeding

- Style -- replace most NULLs

- First modularized version
