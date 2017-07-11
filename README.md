
*Based on "a56 - a DSP56001 assembler - version 1.3"*

```C
/*
 * Copyright (C) 1990-1998 Quinn C. Jensen
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  The author makes no representations
 * about the suitability of this software for any purpose.  It is
 * provided "as is" without express or implied warranty.
 *
 */
```

# Overview

This program was written as a vehicle to learn the intricacies
of the DSP56001 instruction set, and to provide a tool for Unix-based
DSP code development (for those of us without a NeXT machine.)

The assembler probably generates bogus code here and there, and no doubt
does not handle all of the syntax.  We welcome all comments, fixes and 
enhancements.


# Building

Type `make`.

The resulting program, a56, is used as follows:

```
a56 [-b] [-l] [-o output-file] file [...]
```

An assembler listing is sent to the standard-output and an ascii-formatted
object file (a56.out) is produced.  The `-b` option adds binary to the listing.
`-l` causes included files to be listed.  `-o` directs the output to the
specified file rather than the default, a56.out.

A separate program, toomf, converts a56.out into "OMF" format suitable for 
downloading to the 56001 via the sloader.a56 program.

```
toomf < a56.out > file.omf
```


# Syntax

The intent was to provide compatibility with Motorola assembler's syntax.
But since the author did not have Motorola's assembler or its documentation,
it is no doubt far from compatible.  Only a few pseudo-ops are implemented--
probably only partially.

Macros are not supported, except through the use of an external macro
preprocessor, such as /lib/cpp.  To facilitate cpp macro expansion, multiple
assembler statements on a single input line are delimited with an `@`, e.g.:

```C
#define JCHEQ(c,label)	move #c,x0 @cmp x0,a @jeq label

#define JCHNE(c,label)	move #c,x0 @cmp x0,a @jne label
```


## Supported pseudo-ops

The following is a list of the pseudo-ops that are recognized:

```Assembly
<symbol> = <expression>                         ;assign a symbol
<label> EQU <expression>                        ;ditto

ALIGN <number>                                  ;set location pointer
                                                ;to next integral
                                                ;multiple of <number>

ORG <space:> <expression>                       ;new location pointer
ORG <space:> <expression>, <space:> <expression>

DC <dc_list>                                    ;declare constants

DS <number>                                     ;reserve <number>
                                                ;words of space

<label> DSM <number>                            ;reserve space for
                                                ;properly aligned
                                                ;modulo-addressed
                                                ;buffer of size
                                                ;<number>, assigning
                                                ;the aligned starting
                                                ;address to <label>

PAGE <number>, <number>, <number>, <number>     ;ignored

INCLUDE <file>                                  ;file inclusion

END                                             ;end
```

In addition, a `PSECT` pseudo-op was implemented.  It allows program sections
to be defined and bopped in and out of, each with its own location counter and
space.  The Motorola assembler probably does not have this pseudo-op, but no
doubt supports the concept in some way.

```Assembly
PSECT <name> <space:><begin_addr>:<end_addr>    ;define

PSECT <name>                                    ;switch to psect <name>
```

## Examples

Example assembly can be found in the "examples" folder.
