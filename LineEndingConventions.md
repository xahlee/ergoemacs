In our source code, the source code should standandize on unix scheme line ending scheme, and the encoding should be utf-8.

Rationale:

There are 3 newline conventions. unix, Windows, and classic Mac OS.
The classic Mac OS convention, is more or less obsolete since OS X in early 2000s. This left us with unix and Windows choices.

Many unix tools (e.g. grep) chock or behave wrong when line ending is in Windows, and since emacs is heavily tied to unix, our dev use lots of unix tools, we should choose unix line ending as standard for all our files.

For character encoding, we should use Unicode, because that is supported since early 2000s in all practically respects in all major operating systems (Windows, Mac, Linuxes). Use of Unicode makes reading easier instead of ascii hacks (`quot' vs “quote”) and since emacs23 has more or less perfect support.

For a Unicode encoding scheme, we should choose utf-8, because utf-8 is standard among unix/linuxes, and is emacs 23's internal char encoding scheme.