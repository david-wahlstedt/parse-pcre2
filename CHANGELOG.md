# Changelog for `parse-pcre2`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

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
  tree for correct expressions, but ouputs "No parse" otherwise.

- **Main focus of the first version**: To support all of the syntax
  and to have a parser that resembles the grammar of the language.

- **Documentation**: Includes a basic [README.md](README.md) with
  instructions on how to install and run the parser, and some remarks
  on limitations and roadmap for future development.

- **Testing**: Still very crude: see [README.md](README.md).
