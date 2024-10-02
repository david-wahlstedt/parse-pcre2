# parse-pcre2

This is an implementation of a PCRE2 (v 10.45) parser, using
Text.ParserCombinators.ReadP.

It exports a function `parsePCRE` that produces an abstract syntax
tree given a correct PCRE2 pattern.

## Installation

### Install Haskell stack

See for instance
https://docs.haskellstack.org/en/stable/install_and_upgrade/

### Install parse-pcre2

```
git clone git@github.com:david-wahlstedt/parse-pcre2.git
cd parse-pcre2
make install
```

Running `make install` will run the code generation script in
[code_gen/](code_gen/), and then run `stack install`. The reason we
generate some code automatically is due to the long lists of character
properties, with names as well as their shortcuts. This is based on
the output of `pcre2test -LP` and `pcre2test -LS` (this only works for
PCRE2 v 10.44 or higher); these outputs are in the
[pcre2test/](pcre2test/) directory. If you want to generate property
names for a different version of PCRE2, issue those commands and
replace corresponding files' contents with the new outputs.

## Usage

There is a main program that can be called from the command-line, for
testing purposes, that expects standard input. If the input has
several lines, these are considered as part of the pattern to parse.

Examples:
```
echo -n 'a|b.'|parse-pcre2
Alt [ Seq [ Lit 'a' ] , Seq [ Lit 'b' , Chartype CTAny ] ]
```
or from a file (we omit the output of space reasons)
```
parse-pcre2 < examples/date-1.txt
```
or for files with several lines, one regex each:
```
while IFS= read -r line || [ -n "$line" ]; do printf "%s" "$line" | parse-pcre2; done < examples/quoting.txt
```

## Why a PCRE2 parser in Haskell?

In fact, I wanted to write a Haskell program that produces random
strings that match a given PCRE2 expression, a bit like
[genex](https://hackage.haskell.org/package/regex-genex-0.7.0), but
for PCRE2. That requires a parser!

The Haskell regular expressions
[page](https://wiki.haskell.org/Regular_expressions) has an overview
of regex support, and includes C bindings to PCRE. However, the PCRE2
C library doesn't have any function that returns an abstract syntax
tree, in the sense one is used to in functional programming. As far as
I am aware, there is no program or library available that just takes a
PCRE2 expression and returns an abstract syntax tree. There is an
[ANTLR4 grammar for
PCRE](https://github.com/antlr/grammars-v4/blob/master/pcre/PCRE.g4),
but it is not complete, and does not support parsing sensitive to
option settings.



However, we haven't found any parser covering the whole PCRE2
language. We may try to connect this parser with some of the existing
libraries in the future.

## Features

- **Complete PCRE2 syntax**: The parser supports all of PCRE2's syntax
  (with some exceptions regarding the semantics of some option
  settings, and how character encodings are treated, which is work in
  progress).

- **Supports dynamic option settings**: It is possible to turn on and
  off options that affect the parsing rules in arbitrary positions in
  the expression, such as `(?x)` and `(?xx)`, to ignore whitespace and
  treat `#.*\n` as comments. We also support the `(?n)` option to turn
  off (and on again by resetting it) capture group numbering.  The
  `(?| ... )` construction (reset group counters for each alternative)
  is taken into account. Note that most of PCRE's options affect only
  matching, and will just be treated as syntactic constructions in the
  resulting tree by our parser: it is up to the application (the user
  of this parser) to process that further.

- **No matching**: This program does not deal with the semantics of
  PCRE2 in terms of matching, only the syntax; the output of the
  parser is just a syntax tree.

- **Forgiving syntax** Some options for PCRE2 that controls, for
  instance, whether or not to allow empty character classes, are
  always on, but this may change in future versions. In short, some
  features of PCRE2 that forbids certain constructions due to
  semantics, are allowed in our parser, and the idea is that the
  application can take care of it. Another example is character
  classes: The expression `[z-a]` is allowed, and yields a range from
  `z` to `a`, which is not allowed by PCRE2. It is up to the
  application to deal with that.

## PCRE2 sample expressions

The examples in [examples/](examples/) are not yet systematic, but
they cover many kinds of PCRE2 patterns. Some of them are taken from
RFCs and YANG specifications, and some are hand-made for this parser.
Some examples (under directories with names containing substring
`fail`) should fail, but the others should succeed.

## Limitations

- **UTF-8 assumed**: The parser accepts option settings that
  control which encoding the pattern is expected to match, but they
  have no effect. We only deal with UTF-8 code points so far.

- **Error handling**: There is no error handling: the parser just
  answers "No parse" if parse is unsuccessful.

- **Testing**: There are still no automatic tests. We have tested the
  examples in the aforementioned `examples` directory successfully,
  and more.

## Some remarks on the code:

There is certainly room for improvement in the code, from a functional
programming point of view. The current version (as of 2024-08-16) does
not contain any innovations or particularly interesting parts, when it
comes to functional programming per se: the main goal was to have a
working parser, and that seems to be the case!

- **Abstract syntax**: The current abstract syntax is somewhat a
  middle ground between two extremes: 1) retaining all information
  necessary to reconstruct the input PCRE2 pattern exactly as it was
  given, and 2) keeping the minimal amount of information necessary to
  match the same set of strings as the given PCRE2 expression
  matches. The reason for this is a bit arbitrary: it is the first
  that came into mind. We would like the constructors to be self
  documenting, but also not too long. Some of the constructors are
  just copies of the corresponding PCRE2 counterparts, like Unicode
  script names, where others try to convey what they do, like `Many`
  for `*`, `Many1` for `+`, and `RepMin Int` for the `{,N}`
  quantifier. Some of the constructors could perhaps be better as
  records, for instance for character class or lookaround, that have
  different properties such as being positive/negative,
  forward/behind, etc.

- **Higher level functional programming**: Not so much. We have a
  monad transformer that handles state and parsing, though.

- **Module headers**: There are no export lists, and a lot of
  suppressed warnings. This is for convenience, and should perhaps be
  changed.

## Acknowledgments

Special thanks to @Thomas-H, for some nice simplifying contributions
and discussion! Also thanks to @bengtj100 for earlier discussions and
feedback!
