# Changelog for `parse-pcre2`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.0.1 - 2024-09-29

### Added
- **Support for dynamic option setting**: The so-called "internal"
  options `x` and `xx` do now affect the parser. For instance, in
  `(?x:a b(?-x)c d)`, the space between `a` and `b` is ignored, but
  the space between `c` and `d` is part of the pattern. Similar for
  character classes is the `xx` option. One-line comments beginning
  with `#` are also activated by these options, and will also be
  deactivated when the options are turned off.

- **StateT monad transformer on top of ReadP**: To handle option
  settings we use StateT, and to wrap ReadP in it we had to
  reimplement some of ReadP's combinators, in order to lift them and
  preserving the state correctly.

- **Capture group numbers**: Capture groups are now numbered. However,
  the option `(?n)` / "no auto capture", that turns off capture group
  numbering, still has no effect (this will be solved soon). The
  current solution is a post-processing step of the syntax tree, and
  will likely be replaced by using the StateT mechanism, keeping track
  of the numbers during parsing.

- **Octal or backref resolved**: The tricky rule for what is an octal
  escape or a backreference is now correctly interpreted, in the sense
  that if the octal sequence can't be a valid backreference, it is
  treated as an octal character escape.

## 0.1.0.0 - 2024-08-14

### Added
- **Initial Release**: This is the first release of `parse-pcre2`, a
  Haskell library for parsing PCRE2 regular expressions.
- **Basic PCRE2 Syntax Support**: Implements the core functionality to
  parse regular expressions following the PCRE2 syntax. It should
  cover (as far as we are aware) all of the syntactical categories
  according to the
  [pcre2syntax](https://pcre2project.github.io/pcre2/doc/html/pcre2syntax.html)
  and
  [pcre2pattern](https://pcre2project.github.io/pcre2/doc/html/pcre2pattern.html)
  man pages, supporting PCRE2 version 10.45.

  An exception, though, is that option settings have no effect, which
  means that strictly speaking the parse is incorrect as soon as an
  option is set that affects how the expression is to be parsed, e.g.,
  the `(?x)` option, to ignore whitespace and treat `#` as one-line
  comments.

  There is no error handling: the parser produces an abstract syntax
  tree for correct expressions, but outputs "No parse" otherwise.

- **Main focus of the first version**: To support all of the syntax
  and to have a parser that resembles the grammar of the language.

- **Documentation**: Includes a basic [README.md](README.md) with
  instructions on how to install and run the parser, and some remarks
  on limitations and roadmap for future development.

- **Testing**: Still very crude: see [README.md](README.md).
