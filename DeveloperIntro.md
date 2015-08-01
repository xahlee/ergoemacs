Initial writeup: 2009-02-16 Last update: 2009-09-23

# Introduction #

ErgoEmacs is a new emacs distribution that is enhanced from GNU Emacs.

# Plan #

The goal is to have a downloadable binary for Windows, Mac, and perhaps Linux eventually. The binary will have an init file that loads many elisp files, as well as needed binaries for some platforms such as Windows (e.g. unix utils such as grep, find, etc., DLLs for handling images, pdf, etc.). The elisp files will override the emacs default user interface, as well as bundling many modes for handling languages such as PHP, Visual Basic, POV-Ray etc that are not bundled with GNU Emacs.

For any existing emacs installation, people should be able to load the ErgoEmacs files and have the ErgoEmacs behavior.

The plan is subject to minor change depending on how things go. The project is task oriented. Don't want to plan too much but do little.

### Summary ###

• **Goal**: The average programer can download ErgoEmacs, use it, and be productive immediately. They don't have to suffer the learning curve of the “emacs way” just to get started. Emacs's power is still **fully** retained.

• **Method**: A set of elisp files that override emacs's default keybindings and user interface. The elisp files includes many popular programing language modes, and other emacs enhancements that exists on the web but are not bundled in GNU Emacs for one reason or another. On platforms such as Windows, bundle standard shell utils such as grep, find, diff etc, and other binaries necessary for many emacs features to work out of the box (e.g. DLL to handle images, pdfs, spell checking, etc.)

• **100% Compatible with GNU Emacs** The project should be entirely compatible with GNU emacs's latest release. The elisp code for UI enhancement must not be too complex. Most will just be turning on a existing emacs feature, changing a keyboard shortcut, or removing a menu or providing some other elisp commands. No change will be made to the elisp language.

• **Modernization by Emacs's Way**. Changes will be based on emacs's ways of operation, such as using buffers, mini-buffer, spliting windows, hundreds of pre-defined keyboard shortcuts to call the among 3000+ built-in commands. This is not a project to turn emacs into a application similar to a conventional text editor or IDE. There will not be user interface changes that changes emacs to emphasize things such as pop-up dialogues (as in AquaEmacs, Carbon Emacs), opening every file in new window (AquaEmacs), or having the Alt key popup menus (EmacsW32). The “modernization” is just to get rid of emacs's 1980's terminologies, keybindings, and documentation. Using ErgoEmacs should still be like using Emacs.

• **IMPORTANT**. The goal is NOT just to have some elisp files with a README, philosophy, instructions on how to compile or build, for “hackers”. The goal is a binary download for Windows & Mac that work out of the box for average professional programers. (professional programers here is defined as those who's income primarily came from coding or sys admin) A user comes to a URL ([ergoemacs.org](http://ergoemacs.org/)) with a single page where they can click to download, then start using emacs productively for their day job the first day, even if the user's background is just using Windows Notepad. Some good models that illustrates the Not-For-Elite-Geeks-Only are much of Google's services and apps such as Gmail, Google Earth, Google Code..., or Apple's websites and apps such as iTunes, iPhoto, Xcode, ..., or even FireFox, Windows Visual Studio, Notepad++, Eclipse, NetBeans.

## Tasklist for Emacs UI change ##

Here's explicit detail of items to change, listed roughly from most important to less. The importance is roughly based on the ratio “impact” over “ease of fix”. Those in <font color='#006400'>green</font> are completed tasks.

• <font color='#006400'>Create a downloadable binary for Windows and Mac. (The Windows binary is done by David Capello. Mac binary is not done yet, but should be trivial since it is a unix. All needed is to bundle the ErgoEmacs's elisp packages with a emacs binary built for OS X.)</font>

• <font color='#006400'>Update emacs default keybinding. For rationale, see: <a href='http://xahlee.org/emacs/emacs_kb_shortcuts_pain.html'>Why Emacs's Keyboard Shortcuts Are Painful</a>. Implemented in ErgoEmacs Keybinding.</font>

• <font color='#006400'>Have Z X C V as default shortcut for undo, cut, copy, paste. Implemented in ErgoEmacs Keybinding.</font>

• <font color='#006400'>Support the minimum of standard shortcut set, for Open, Close, Save, Save As, New, Find, Print, Select All. Implemented in ErgoEmacs Keybinding.</font>

• <font color='#006400'>Have text selection highlight. Just need to turn on cua-mode, transient-mark-mode, and delete-selection-mode. (transient-mark-mode is on in emacs 23.)</font>

• <font color='#006400'>Have up/down arrow key move by visual line. (emacs 23 does this by default)</font>

• <font color='#006400'>Change undo and redo to behave like modern applications. Rationale: see the faq section at: <a href='http://xahlee.org/emacs/modernization.html'>The Modernization of Emacs</a>. (implemented by including the redo.el package. Though, technically the redo.el package has some problems. It sometimes corrupt undo record or your buffer. However, redo mode is considered good enough for ErgoEmacs for the moment.)</font>

• <font color='#006400'>Remove menu items that are redundant, never used, outdated. Add menu items that are common to all modern apps. Rationale: <a href='http://xahlee.org/emacs/modernization_menu.html'>Emacs's Menu Usability Problem</a></font>

• Replace the shortcut notation C-‹key› and M-‹key› by Ctrl+‹key› and Alt+‹key› in all menus and inline doc. Rationale: <a href='http://xahlee.org/emacs/modernization_meta_key.html'>Emacs's M-‹key› Notation vs Alt+‹key› Notation</a>. This is currently done for menus (much thanks to David Capello). Still needs to be done for describe-function.

• <font color='#006400'>Replace list-buffers with ibuffer. This means, the keybinding for list-buffers should now call ibuffer, and the menu item of List Buffer should call ibuffer instead too. Rationale: ibuffer replaces and enhances all functionality of list-buffer. (See: <a href='http://xahlee.org/emacs/effective_emacs.html'>Tips on Long Term Emacs Productivity</a>.)</font>

• Fix emacs mode line. Rationale: <a href='http://xahlee.org/emacs/modernization_mode_line.html'>Emacs's Mode Line Modernization Suggestions</a>. Probably not too difficult to do. Need to look into how mode line works.

• Get rid of the “scratch” buffer. Not trivial. Rationale: <a href='http://xahlee.org/emacs/modernization_scratch_buffer.html'>Suggestions on Emacs's Scratch Buffer</a>.

• <font color='#006400'>For Windows, bundle unix utilities that are usually required for emacs's many features to work. Roughly in order of importance: hunspell/aspell, grep, diff, find, patch. Possibly also bash, xargs, sed, gawk, mv, cp. Especially important is that spell checking must work out of the box. (Much thanks to David Capello)</font>

### Bundle Additional Emacs Packages ###

• <font color='#006400'>Include modes for most popular langs. css , php-mode, Visual Basic, javascript. Rationale: the number of programers of these langs probably account for more than 50% of the number of all professional coders. Emacs doesn't have a mode for these out of the box is a major problem. (emacs 23 bundled css mode. Bundling a mode for javascript is planned for GNU Emacs 23.2. GNU Emacs developers doesn't mind including php-mode but it does not seem critical to them. And, one of the current GNU Emacs project leader (Stephen) explicitly expressed that GNU Emacs will not bundle a mode for Visual Basic.)</font>

• <font color='#006400'>Include less used programing lang modes: AutoHotKey mode (a popular kbd macro system for Windows), POV-Ray mode, PowerShell lang mode, PowerShell interactive shell, OCaml/F#.</font>

• <font color='#006400'>Include better modes than those bundled with emacs. e.g. nxml-mode for xml (done in Gnu Emacs 23), speck-mode for spell checking, yasnippet template system, and others...</font>

## Patch vs elisp script add-on ##

There are 2 ways to change emacs behavior: (1) provided a add-on elisp scripts, which loads when emacs starts and override emacs behavior. (2) patch emacs's elisp files or C code.

The add-on elisp approach should be used, unless it cannot fix a issue, or such method of fixing is too complex or causes too much slowdown. A example where patching may be necessary: having describe-function or describe-key display “Ctrl+‹key›” instead of “C-‹key›” notation.

The reason that elisp add-on is preferred is because it is easier, and much easier to maintain compatibility with GNU Emacs, since it is just a layer on top. It probably does not introduce any human noticeable speed penalty or other problem.

If we do patch emacs, it must be done in some simple, mechanically automatable way. e.g. using make files and diff and patch util etc with strict formats. The goal is that when new emacs version comes out, it should be no more than half a day's work to make ErgoEmacs work with new emacs 100%.

## Emacs Manual Changes ##

The manual change would be low priority, because user should not be expected to have to read manual in order to know how to do most editing tasks. **Important**: as a modern software, the UI should be designed such that a “manual” is not necessary for most operations. Software User Manual is popular in 1980s and 1990s, but such concept is getting obsolete, especially bulky manuals. (think of just about all of Google's apps and services (e.g. Gmail, iGoogle, GoogleEarth, GoogleMap, blogger, picassa, GoogleCode, ...), and Apple computer's software (e.g. iTune, iPhoto, iChat...).)

This is why manual change is not high priority. Fixing unintuitive, complex, UI has priority. When done well, manual is not much needed, or needed only to give tech details or outline some general principle (such as that every keystroke in emacs is actually just invoking a command; concepts of major mode, minor mode, the paste-board ring, etc.).

Editing and rewriting a manual is not difficult, however, there's much difficulty in the emacs case due to mostly social reasons. Emacs manual is done in Texinfo, which is a advanced documentation technology in the 1980s, and is outdated by about decade considered today. Still, it works fine in practice, especially for plain text contexts. But, getting the changed manual back to FSF is near impossible for social reasons. It will involve huge effort in discussion with the FSF developers and passing Richard Stallman. The persuasion and discussion effort can easily be more than the effort to actually fix the manual. If persuasion is not successful and we still want to fix the doc , that means, for each release of emacs and its manual, the rework on manual almost have to start fresh again.

For now, here's some items as a record of what should be changed in the Emacs manual:

• Replace all terms “Meta key” to “Alt key”. Rationale: <a href='http://xahlee.org/emacs/modernization_meta_key.html'>Emacs's M-‹key› Notation vs Alt+‹key› Notation</a>.

• Change the terminology of kill to cut, yank to paste, kill-ring-save to copy, throughout the manual. Easy to fix. Note: this is not about changing command names, but only terms used in manual that are used to describe activity of copy, cut, paste.

• Change the terminology of “keybinding” to “keyboard shortcut” or possibly just “hotkey”. Only when discussing “assign a shortcut” for programing contexts (read by those who wants to customize, program, emacs), then use “binding” or “keybinding”. (this item is easy to fix.)

• Switch the term “Window” and “Frame”. This will take some diligence to fix this correctly. The logic after this change is this: for elisp coders or power users, they will learn that in elisp the term “window” and “frame” are switched from conventional understanding , and this is due to historical reasons. For most users who are not coding elisp, they wouldn't need to be confused by this terminology switch. (thanks to Michael Ekstrand for some suggestion on this)

• Replace terms like “mouse 1”, “mouse 2”, “mouse 3”, to left button, middle button, right button.

• Replace outdated stuff like mentioning of DOS, VMS, and others that are practically obsolete for a decade or more.

## Less Important ##

The following are less important, their ratio of “impact”/“ease to fix” is not high.

• Have dired do automatic dir update, with a option in customize to turn auto update off. Not trivial. Look into how xemacs did it, and several existing elisp files that attempted to do this.

• <font color='#006400'>fix emacs letter case change commands. Code in ErgoEmacs. <a href='http://xahlee.org/emacs/modernization_upcase-word.html'>Usability Problems With Emacs's Letter-Case Commands</a></font>

• Extend Selection By Semantics feature. (Replaces emacs's various mark- commands) Partially done. <a href='http://xahlee.org/emacs/modernization_mark-word.html'>Suggestions on Emacs's mark-word Command</a>. The plan is that this a kbd shortcut should progressively select larger semantic unit.  <a href='http://xahlee.org/emacs/syntax_tree_walk.html'>Extend Selection By Semantic Unit</a>.

This is a item that is rather kinda a revolutionary feature, but is hard to implement (involves non-trivial parsing on many languages). When done correctly, “impact” is very high, but the “ease to fix” is probably even higher.

• Auto-format feature. (supplant emacs's fill- commands) See <a href='http://xahlee.org/emacs/modernization_fill-paragraph.html'>Suggestions on Emacs's Line-Cutting Commands</a>.

The plan is that user should just press a button to invoke a reformat command that reformat the semantic unit the cursor is on, or a text selection if there is one. A semantic unit can be a paragraph of English, as done by fill-paragraph, or it can be a block of code, of any language with C-like syntax or complete regular syntax (such as lisp). A block of code in C-like langs is delimited by {}, or a function definition, or delimited by two newline chars.

For some detail on how this would work for lisp code, see: <a href='http://xahlee.org/emacs/lisp_formatter.html'>A Simple Lisp Code Formater</a>. Andy Stewart wrote a lisp formater for this (see emacswiki) but currently isn't quite usable in our context.

In summary, there should be a single shortcut, that calls a single command, that smartly does one of fill-paragragh, or fill-region, or fill-buffer, depending on context, except these fill commands works not just for English text, but for any programing language with a C-like syntax (C, C++, Java, C#, JavaScript ...), as well as regular syntax languages like lisp and XML, and the result should be a nicely reformatted text or code.

This single shortcut would replaces the tens of formatting commands related to indenting in programing language modes. When this shortcut is pressed twice, it'll cycle thru some pre-defined formatting, such between compact (no linebreaks) and some “standard” of that lang. User should be able to set preferences on what is meant by “compact” or “standard”, of course.

This is again a revolutionary feature. The impact is high, but “ease of fix” is high too.

• elisp's describe-function should work in all languages, not just lisp, and it should work out of the box. (in a sense, emacs becomes the interface center to **ALL** popular programing language's function's inline documentation) If the lang's doc does not come in info format, send user to browser with pertinent url. Partially done. A bit work to fix this since this requires patch or wrapper that works for all major langs, by first finding out the major mode, then the lang. Some lang's mode already provide some form of this, but their UI are not incompatible. For some explanation and example on this, see: <a href='http://xahlee.org/emacs/emacs_lookup_ref.html'>Dictionary and Reference Lookup with Emacs</a>.

Again, a killer feature, but the effort to complete this is a bit large, since it needs to cover about 6 or so languages, but this is easy to implement, just that the quantity of work is largish.

In this page, the term “major languages” or “popular languages” is roughly defined as by number of coders for that lang. Roughly, it is this list: C, C++, Java, perl, python, php, visual basic, javascript, html, css, xml, c#, TeX/LaTeX. For some reference, see: <a href='http://www.tiobe.com/index.php/content/paperinfo/tpci/index.html'><a href='http://www.tiobe.com/index.php/content/paperinfo/tpci/index.html'>http://www.tiobe.com/index.php/content/paperinfo/tpci/index.html</a></a>.

### small items ###

• <font color='#006400'>elisp-index-search should use the word under cursor as the default choice in prompt. See: <a href='http://xahlee.org/emacs/modernization_inline_doc.html'>Suggestions on Emacs's Inline Doc</a></font>

• describe-function should include links to appropriate sections in emacs manual, elisp manual, or the mode's manual. See: <a href='http://xahlee.org/emacs/modernization_inline_doc.html'>Suggestions on Emacs's Inline Doc</a>

• <font color='#006400'>Change Meta+Tab for command name completion to some other key. The Meta is Alt, and Alt+Tab is used by Windows, Mac, Linux for switching apps. (the fix is implemented in the ErgoEmacs keybinding package as experimental. The key is Alt+t.).</font>

## Remote Future Plans ##

Here are more items, they are in general just fancy thinking. Listed here only as a record for communication of general ideas for this project.

• Something urgently needs to be done with emacs html mode, for pure html as well as mixed css/html/php/js. This is a MAJOR need, because web app programing is probably the largest sector of programing activities today. Emacs today does not support mixed css/html/php/js well at all (savy emacs and elisp coders can spend days to download and setup and fix the various packages on the web to do this yet the result isn't very good). The fix is very difficult. See <a href='http://xahlee.org/emacs/emacs_html_sucks.html'>Emacs's HTML Mode Sucks</a>. nxhtml mode is currently work in progress by emacs dev, good luck to Lennart.

• Reduce emacs documentation by at least 1/3 of its bulk. Remove all things modern users already know, such as concepts of copy, cut, paste, paste board, opening files... etc. e.g. remove or reduce manual nodes on: Basic, Minibuffer, M-x, Help, Mark, Killing, Yanking, Accumulating Text, Major Modes, Indentation, Text, ... just about every node in emacs manual. Most of them can be replaced with one single paragraph, and possibly another single paragraph on tech detail when appropriate (such as command names, elisp code samples, things having to do with the emacs system). See: <a href='http://xahlee.org/emacs/emacs_manual_problem.html'>Problems of Emacs's Manual</a>.

• Provide interface to gmail, yahoo, msn mail. (kinda as a replacement of rmail's role in emacs) Also provide interface to google groups. (can be added as part of gnus) Rationale: <a href='http://xahlee.org/emacs/modernization_html_mail.html'>Emacs Should Support HTML Mail</a>. Web based email are modern ways. Only a very small minority of people are using plain text clients today. Rmail & gnus stays in emacs, of course, for old timers.

• Bundle e-blog and lj-update for easy update to google blogs and livejournal. • possibly bundle nxhtml mode and quite a few others really good ones and important, some of which are likely never to be in GNU emacs due to author unwilling or other.

• Translate docs from texinfo to html. A eventual ideal. <a href='http://xahlee.org/emacs/modernization_html_vs_info.html'>Emacs Should Adopt HTML To Replace Texinfo</a>.

## GNU Emacs ##

About half of these ideas are officially communicated to gnu emacs's dev thru emacs bug report system, in 2007, 2008, 2009. Almost all are either placed on wish list, or dismissed as unimportant or unnecessary. Some emacs 23 changes, such as moving up/down by visual line, and transient-mark-mode on by default, might be influenced by some of the essay linked here. But overall, for many reasons the above ideas are not likely to show up in GNU Emacs.

If you think these are good ideas, either send in a “bug” report to FSF in emacs thru the menu “Help ‣ Send Bug Report...”, or help contribute here.

## Existing Similar Projects ##

There are a number of existing projects that are somewhat similar to the spirit of “modernization” here.

• **AquaEmacs** is fantastic. However, it runs on Mac only. This is a major problem for getting most programers to adopt emacs, because Mac is only used by less that 10% of computer users. Also, some of its Mac GUI behavior is not suitable for emacs. For example, popping up dialog boxes when user tries to open a file. Opening every file in its own window (what emacs calls “frame”), launching Mac OS X's (proprietary) Helper Application when pulling some help menu item. In the end, AquaEmacs's ways makes emacs unsable as a text terminal application, and destroys much of emacs's power in keyboard heavy operations.

• **EmacsW32** (officially called “EmacsW32 Utilities”) is officially a bunch of Windows utilities such that when used together with GNU Emacs for Windows, makes emacs a much better Windows software. (in practice, this project is just called EmacsW32 by people, and is seen simply as easy-to-use binary distribution of Emacs for Microsoft Windows.)

EmacsW32 is great, but there are 2 major problems:

(1): The project is officially thought of as a emacs add-on called “EmacsW32 Utilities”, not a complete software you can just download and use. The website design is a lot more complex than necessary, and the writing is pretty bad. (you can just compare the home page of EmacsW32 and Aquamacs) It requires users to read a bunch of philosophies about emacs or why EmacsW32 exists, reading technicalities about what EmacsW32 is or is not, with complex download choices about versions to download such as “with patch”, “without patch”, “installer vs zipped” download, “source code only”, etc. The English writing on the site is also pretty lousy (because Lennart is not a native English speaker), the site's pages or url are organized in a way that's often confusing.  Because of the above, the EmacsW32 project is primarily for people who are ALREADY EMACS USERS. For vast majority of professional programers, EmacsW32 doesn't interest them.

(2): The Alt key pops up the Window's menu. This is how app behaves in MS Windows. This is great for Windows compatibility just as AquaEmacs is great for Mac compatibility, but this operation is incompatible with emacs's ways, where Meta is heavily used thus convenient to be at the location of Alt key.

• **Easymacs**. Easymacs is again a bunch of patches, with complex info about how to install, where to collect needed packages, philosophy, manifesto. This project seems also to be not maintained.

The above projects do not attempt to fix the emacs's badly designed keybinding problem. Nor do they attempts to fix emacs's out-dated terminologies problem (AquaEmacs tries, by writing its own Mac style docs launched in Apple's Help Application, which function as layer that relies on top of GNU Emacs manual.)

It would be ideal to talk to the above authors and unify the work and ideas into a single coherent emacs modernization design with cooperative efforts. In practice, that is much impossible. Aquamacs author is probably not interested in merging its interface or code base with EmacsW32, and vice versa, further, these are on different platforms, so acting as 2 separate projects both using GNU Emacs as base is probably more efficient. ErgoEmacs ideas about modernized keybinding and modernization of emacs based on emacs's ways, is probably not interested by the above authors neither. As a separate project, it would move faster and probably more beneficial to the whole emacs community on the whole.

One most critical aspect of ease of use is a brainless downloadable binary. This is done for essentially EVERY major software (think of all major browsers, every software released by Apple, every software released by Microsoft, every gaming software, and every software released for Windows.) However, emacs, much with its unix history, has the mentality that user is to compile the software himself. Downloadable binary is maybe available for 1% of time, and when available, often outdated or crash. The only exception to this, is the Mac version of emacs (Carbon Emacs, AquaEmacs), which is that way due to the Mac culture. However, they only run on Mac. Some 95% of computer users can't use these. (because ~95% computer users use Windows).