Multilingualization for OCaml source code
=========================================

The _m17n_ package allows using Unicode identifiers in the OCaml source code.

``` ocaml
type 色 =
| @赤
| @黄色
| @緑色
[@@deriving show]

let () = print_endline (show_色 @赤)
```

Installation
------------

Because of [PR6695][], it is necessary to use a patched version of the OCaml
compiler. Thankfully, installing it (and _master_ versions of some
supplementary packages) with [OPAM 1.2][opam] is as simple as:

``` sh
opam switch 4.03+pr???
opam pin add -y m17n git://github.com/whitequark/ocaml-m17n
opam pin add ppx_tools git://github.com/whitequark/ppx_tools
```

Note that _m17n_ is not compatible with [camlp4][].

[PR6695]: http://caml.inria.fr/mantis/view.php?id=6695
[opam]: https://opam.ocaml.org
[camlp4]: https://github.com/ocaml/camlp4/

Usage
-----

_m17n_ can be activated using [ocamlfind][]:

``` sh
ocamlfind ocamlc -package m17n -syntax utf8 ...
```

If you are using [ocamlbuild][], add the following to your `_tags` file:

```
<**/*.{ml,mli}>: package(m17n), syntax(utf8)
```

_m17n_ also works in toplevel. It can be activated using:

```
#require "m17n";;
```

[ocamlfind]: http://projects.camlcity.org/projects/findlib.html
[ocamlbuild]: http://nicolaspouillard.fr/ocamlbuild/ocamlbuild-user-guide.html

Features
--------

_m17n_ expects the source code to be valid UTF-8. It extends the identifiers
normally recognized by OCaml to include all Unicode letters and digits.
The case distinction is also preserved; however, the files corresponding to
modules with non-English names must have the first character to be uppercase.

Since few of the world's scripts distinguish between upper and lower case,
a sigil is provided to disambiguate constructor and module names and all
other identifiers. When an identifier is prepended with `@`, it is treated
as if its first letter was uppercase.

See [technical details](#technical-details) for specifics.

_m17n_ is compatible with ppx syntax extensions such as [ppx_deriving][].

_m17n_ does not add Unicode literals or any runtime support for manipulating
Unicode strings and characters. (See [OCaml pull #80][pr-uchar], [Uutf][], [Uunf][]
and [Uucd][] projects.)

[ppx_deriving]: https://github.com/whitequark/ppx_deriving
[pr-uchar]: https://github.com/ocaml/ocaml/pull/80
[uutf]: http://erratique.ch/software/uutf
[uunf]: http://erratique.ch/software/uunf
[uucd]: http://erratique.ch/software/uucd

### Can't look-alike characters like a and а be confusing?

They can. However, _m17n_ issues a warning if more than one script
is used in an identifier, hopefully handling most of the confusing
cases.

### Localized error messages?

This will be possible when [PR6696] is fixed.

[PR6696]: http://caml.inria.fr/mantis/view.php?id=6696

### Localized keywords?

It is trivial to add support for these. What's hard is coming up with
a good localization. I tried to make one, but committing it felt
worse than kicking a bucket of kittens. If you have one, please
[open an issue][issue].

### Are RTL scripts supported?

In theory, yes. However I lack ability to verify whether the RTL support
works correctly. [Open an issue][issue] if it is not.

[issue]: https://github.com/whitequark/ocaml-m17n/issues

Technical details
-----------------

### Unicode handling

_m17n_ includes only five changes to the OCaml lexer:

  * U+0009 CHARACTER TABULATION, U+000A LINE FEED (LF), U+000D CARRIAGE RETURN (CR),
    U+000C FORM FEED (FF), U+0020 SPACE and U+3000 IDEOGRAPHIC SPACE are
    recognized as whitespace,
  * Characters of [General Category][gc] [Lu][gcv] are recognized as uppercase letters
    at the start of an identifier,
  * Characters of [General Categories][gc] [Ll][gcv], [Lm][gcv], [Lo][gcv], [Lt][gcv] and
    U+005F LOW LINE are recognized as lowercase letters at the start of an identifier,
  * Characters with property [ID_Continue][d1] are recognized as continuation of
    an identifier,
  * U+0040 COMMERCIAL AT makes the following lowercase or unicase letter recognized
    as an uppercase letter. If it is possible to fold the letter to upper case,
    this is done.

To summarize, an identifier may start with [ID_Start][d1], and continue
with [ID_Continue][d1].

All identifiers are normalized to [NFC][nf]. However, strings are not normalized.

These rules closely follow the recommendations of [Unicode TR31][tr31].
Specifically, _m17n_ conforms to Unicode 6.3 [UAX31 Level 1][C2], observing [R1][]
and [R3][] with the profile specified above and [R4][] unconditionally
with normalization to [NFC][nf].

[gc]: http://www.unicode.org/reports/tr44/#General_Category
[gcv]: http://www.unicode.org/reports/tr44/#General_Category_Values
[d1]: http://unicode.org/reports/tr31/#Default_Identifier_Syntax
[nf]: http://www.unicode.org/reports/tr15/#Norm_Forms
[c2]: http://unicode.org/reports/tr31/#C2
[r1]: http://unicode.org/reports/tr31/#R1
[r3]: http://unicode.org/reports/tr31/#R3
[r4]: http://unicode.org/reports/tr31/#R4
[tr31]: http://unicode.org/reports/tr31/

### Interaction with filesystem

OCaml uses module names to search the include path for referenced modules.
As the module names are normalized to [NFC][nf], the queries to the filesystem
use the same form. Different operating systems handle them in different ways:

  * Mac OS X on HFS+ stores the filenames in [NFD][nf] and normalizes all input
    to [NFD][nf]. No edge cases possible.
  * Other *nix systems such as Linux treat filenames as opaque `/`-delimited,
    `NUL`-finalized streams of bytes, however essentially all existing input
    methods normalize to [NFC][nf].
  * Windows treats filenames as opaque streams of UTF-16 characters with
    somewhat [more complex][winfn] interpretation. It performs its own
    case folding (in most cases; case-sensitive Windows filesystems
    [exist][wincs]), but no normalization. Its input methods normalize to
    [NFC][nf] as well.

_m17n_ aims to reduce possible confusion by looking into the include
directories and looking for any OCaml build products whose basenames are
identical to the names of any referenced modules under toNFKC_Casefold
(definition R5 in [Unicode 6.3 section 3.13][u63]), but not as-is.
This measure should be enough to not only catch all instances of
mis-normalized filenames, but also incorrect capitalization and look-alike
characters.

[winfn]: http://msdn.microsoft.com/en-us/library/aa365247(v=VS.85).aspx
[wincs]: https://support.microsoft.com/KB/100625
[u63]: http://www.unicode.org/versions/Unicode6.2.0/ch03.pdf

### Interaction with the OCaml compiler

The OCaml compiler has an `-pp` option which, among other things, allows
to provide a binary that accepts source code and emits an OCaml abstract
syntax tree, thus allowing to implement a custom frontend. (This is
what [camlp4][] uses.)

Additionally, the OCaml compiler exports its internals, including
the parser, in a package `compiler-libs`, thus allowing to avoid
reimplementing the parser in a custom frontend.

The compiler treats the identifiers as opaque tokens almost everywhere.
It does not even concatenate them, which is important, as [NFC][nf]
[is not generally closed under concatenation][nfnotclosed]. The only place
where the compiler actually dissects the strings is the module name → filename
mapping. However, it ignores bytes with values over 127, passing UTF-8
strings through.

Findlib provides an interface that allows registering a preprocessor.
Additionally, it will pass all package include paths to such a preprocessor.

_m17n_ uses all these features and implementation details to provide
a seamless Unicode-aware frontend.

[nfnotclosed]: http://www.unicode.org/reports/tr15/#Concatenation

License
-------

[MIT license](LICENSE.txt)
