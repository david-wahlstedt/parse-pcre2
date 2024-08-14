# parse-pcre2

This is an implementation of a PCRE2 (v 10.45) parser, using
Text.ParserCombinators.ReadP

## Installation

### Install Dependencies

Install Haskell stack
https://docs.haskellstack.org/en/stable/install_and_upgrade/

### Install parse-pcre2

```
git clone git@github.com:david-wahlstedt/parse-pcre2.git
cd parse-pcre2
make install
```

The reason we use a Makefile is that some of the source files are
automatically generated from the output of `pcre2test -LP` and
`pcre2test -LS` (this only works for PCRE2 v 10.45 or higher). If you
want to generate property names for a newer version of PCRE2, issue
those commands and put the output in the corresponding files in the
`pcre2test` directory.

## Usage

Example:
```
parse-pcre2 < examples/date-1.txt
```

## Examples

The examples in `examples/` are not yet systematic, but they cover
many kinds of PCRE2 patterns. Some of them are taken from RFC's and
YANG specifications, and some are hand-made for this parser.
Some examples should fail, but most of them should succeed.
