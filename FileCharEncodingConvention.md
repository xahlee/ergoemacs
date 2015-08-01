Source code character encoding should be utf-8.

Rationale:

unicode today is supported on Windows, Mac out of the box in all apps and robustly since maybe early 2000s. And i think most liuxes today has that status too. Many languages and computing standards, by today specify unicode as its char set. (e.g. Java, XML)

Windows uses utf-16 for files by default.  Mac uses utf-8. For linuxes, also utf-8.

Emacs should use unicode by default, and the encoding choice should be utf-8, because most unix tools still deal with ascii only, and utf-8 is compatible with ascii if there's no unicode chars. Also, emacs 23 uses utf-8 for all internal char encoding.

Thanks to David Capello and Francesco Biccari for the idea.