;;; xah-elisp-mode.el --- Major mode for editing emacs lisp. -*- coding: utf-8 -*-

;; Copyright © 2013 by Xah Lee

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;; Created: 2013-03-23
;; Keywords: languages, convenience

;; LICENSE:
;; paypal me $5
;; Buy Xah Emacs Tutorial
;; Please see: http://ergoemacs.org/emacs/xah-elisp-mode.html

;;; Commentary:
;; Major mode for editing emacs lisp.
;; See: http://ergoemacs.org/emacs/xah-elisp-mode.html

;; todo
;; 2014-06-30 don't expand abbrev (push-mark▮)

;;; History:
;; version 0.5, 2014-06-23 first “polished” release.
;; version 0.1, 2013-03-23 first version

(require 'lisp-mode)

(defvar xah-elisp-mode-hook nil "Standard hook for `xah-elisp-mode'")

(defvar xah-elisp-elisp-lang-words nil "List of elisp keyword more or less related to elisp the language.")
(setq xah-elisp-elisp-lang-words '(

"read"
"eval"
"intern"

"zerop"
"listp"
"numberp"
"functionp"
"char-equal"

"delete"
"make-list"
"memq"
"delq"
"remq"
"memql"
"member"
"remove"
"member-ignore-case"
"delete-dups"

"abs"

"url-unhex-string"
"decode-coding-string"
"add-to-list"
"and"
"append"
"apply"
"aref"
"aset"
"assoc"
"assq"
"boundp"
"car"
"catch"
"cdr"
"char-to-string"
"commandp"
"concat"
"cond"
"condition-case"
"cons"
"consp"
"defmacro"
"defun"
"elt"
"eq"
"equal"
"expt"
"fboundp"
"featurep"
"floatp"
"format"
"format-time-string"
"funcall"
"function"
"get"
"gethash"
"hash-table-count"
"if"
"integerp"
"lambda"
"last"
"length"
"let"
"let*"
"list"
"load"
"make-hash-table"
"mapc"
"mapcar"
"mapconcat"
"maphash"
"max"
"message"
"min"
"nil"
"not"
"nth"
"nthcdr"
"null"
"number-sequence"
"number-to-string"
"or"
"pop"
"prin1"
"princ"
"print"
"progn"
"provide"
"push"
"put"
"puthash"
"quote"
"random"
"rassoc"
"regexp-opt"
"regexp-quote"
"remhash"
"require"
"reverse"
"set"
"setq"
"sleep-for"
"sort"
"split-string"
"string"
"string-equal"
"string-match"
"string-match-p"
"string-to-number"
"string="
"stringp"
"subrp"
"substring"
"symbol-function"
"symbol-name"
"symbol-plist"
"symbol-value"
"symbolp"
"t"
"throw"
"unless"
"vector"
"when"
"while"
"defvar"
"float"
"vectorp"
"vconcat"
"car-safe"

"print-level"
"print-length"
))

(defvar xah-elisp-emacs-words nil "List of keywords that are not related to emacs lisp the language.")
(setq xah-elisp-emacs-words '(

"count-matches"
"delete-and-extract-region"

"define-abbrev-table"
"current-word"
"call-interactively"
"left-char"
"right-char"

"propertize"
"setq-local"
"upcase"
"upcase-region"
"downcase-region"
"downcase"
"capitalize"
"upcase-initials"

"atomic-change-group"
"ido-completing-read"
"ido-read-directory-name"

"terpri"
"match-string-no-properties"

"frame-parameter"
"frame-parameters"
"modify-frame-parameters"
"set-frame-parameter"
"modify-all-frames-parameters"

"with-syntax-table"

"buffer-enable-undo"
"buffer-disable-undo"

"define-minor-mode"
"set-buffer-modified-p"
"file-readable-p"
"buffer-live-p"

"parse-partial-sexp"
"skip-syntax-forward"
"skip-syntax-backward"
"forward-comment"
"scan-lists"
"scan-sexps"
"syntax-ppss"
"forward-symbol"
"forward-sexp"
"backward-up-list"

"set-default"

"add-hook"
"autoload"
"backward-char"
"beginning-of-line"
"bounds-of-thing-at-point"
"buffer-file-name"
"buffer-modified-p"
"buffer-substring"
"buffer-substring-no-properties"
"called-interactively-p"
"completing-read"
"copy-directory"
"copy-file"
"current-buffer"
"custom-autoload"
"custom-set-faces"
"defalias"
"defconst"
"defcustom"
"defgroup"
"define-derived-mode"
"define-key"
"defsubst"
"delete-char"
"delete-directory"
"delete-file"
"delete-region"
"directory-files"
"dolist"
"dotimes"
"end-of-line"
"error"
"expand-file-name"
"file-directory-p"
"file-exists-p"
"file-name-directory"
"file-name-extension"
"file-name-nondirectory"
"file-name-sans-extension"
"file-regular-p"
"file-relative-name"
"find-file"
"forward-char"
"forward-line"
"generate-new-buffer"
"global-unset-key"
"goto-char"
"insert-file-contents"
"insert"
"interactive"
"kill-region"
"kill-ring-save"
"kill-all-local-variables"
"kill-buffer"
"kill-new"
"line-beginning-position"
"line-end-position"
"local-set-key"
"looking-at"
"make-directory"
"make-local-variable"
"mark"
"match-beginning"
"match-end"
"match-data"
"match-string"
"narrow-to-region"
"point"
"point-marker"
"point-max"
"point-min"
"pop-mark"
"prefix-numeric-value"
"push-mark"
"re-search-backward"
"re-search-forward"
"read-directory-name"
"read-file-name"
"read-from-minibuffer"
"read-regexp"
"read-string"
"region-active-p"
"use-region-p"
"region-beginning"
"region-end"
"remove-hook"
"rename-file"
"repeat"
"replace-match"
"replace-regexp"
"replace-regexp-in-string"
"run-hooks"
"save-buffer"
"save-excursion"
"save-restriction"
"search-backward"
"search-backward-regexp"
"search-forward"
"search-forward-regexp"
"set-buffer"
"set-file-modes"
"set-mark"
"shell-command"
"skip-chars-backward"
"skip-chars-forward"
"substring-no-properties"
"thing-at-point"
"user-error"
"widget-get"
"with-current-buffer"
"with-temp-buffer"
"with-temp-file"
"write-file"
"write-region"
"y-or-n-p"
"yes-or-no-p"

"setenv"
"getenv"
"modify-syntax-entry"
"make-sparse-keymap"
"standard-syntax-table"
"run-mode-hooks"
"set-syntax-table"
"use-local-map"
"defface"

"shell-command-to-string"

"start-process"

"next-buffer"
"previous-buffer"
"buffer-name"
"insert-buffer-substring-no-properties"
"erase-buffer"
"append-to-file"
"buffer-string"

"switch-to-buffer"
"with-output-to-temp-buffer"
"setq-default"

"parse-time-string"

"bolp"
"eolp"
"eobp"
"bobp"
"char-after"
"char-before"
"following-char"
"preceding-char"
"get-char-property"

"overlay-get"
"overlay-put"
"overlay-properties"

"overlayp"
"make-overlay"
"overlay-start"
"overlay-end"
"overlay-buffer"
"delete-overlay"
"move-overlay"
"remove-overlays"
"copy-overlay"
"overlay-recenter"
"overlays-at"
"overlays-in"
"next-overlay-change"
"previous-overlay-change"
"redraw-frame"
"selected-frame"
"set-window-margins"
"variable-pitch-mode"
"window-body-width"
"window-margins"
"set-fontset-font"
"font-family-list"

"add-to-invisibility-spec"
"remove-from-invisibility-spec"
"invisible-p"
"number-or-marker-p"
"version<"
"version<="
"version"
"emacs-version"

))

(defvar xah-elisp-emacs-user-commands nil "list of keywords related to user's needs.")
(setq xah-elisp-emacs-user-commands '(

"repeat-complex-command"

"clear-rectangle"
"complete-symbol"
"define-prefix-command"
"delete-rectangle"
"delete-whitespace-rectangle"
"electric-indent-local-mode"
"electric-indent-mode"
"electric-layout-mode"
"electric-pair-mode"
"eval-buffer"
"eval-defun"
"eval-expression"
"eval-last-sexp"
"eval-region"
"global-set-key"
"kbd"
"key-translation-map"
"kill-rectangle"
"linum-mode"
"open-rectangle"
"prettify-symbols-mode"
"rectangle-mark-mode"
"rectangle-number-lines"
"replace-rectangle"
"shell-command-on-region"
"shell-command-on-region"
"sort-lines"
"subword-mode"
"yank-rectangle"

  ))

(defvar xah-elisp-keyword-builtin nil "List of elisp names")
(setq xah-elisp-keyword-builtin '( "&optional"))

(defvar xah-elisp-elisp-vars-1 nil "List elisp variables names")
(setq xah-elisp-elisp-vars-1 '(

"current-prefix-arg"
"deactivate-mark"
"load-file-name"
"buffer-file-name"
"load-path"

"Buffer-menu-buffer+size-width"
"Buffer-menu-mode-width"
"Buffer-menu-name-width"
"Buffer-menu-size-width"
"Buffer-menu-use-frame-buffer-list"
"Buffer-menu-use-header-line"
"Info-default-directory-list"
"Info-split-threshold"
"abbrev-all-caps"
"abbrev-file-name"
"ad-default-compilation-action"
"ad-redefinition-action"
"adaptive-fill-first-line-regexp"
"adaptive-fill-function"
"adaptive-fill-mode"
"adaptive-fill-regexp"
"add-log-current-defun-function"
"add-log-full-name"
"add-log-mailing-address"
"after-save-hook"
"allout-auto-activation"
"allout-widgets-auto-activation"
"apropos-compact-layout"
"apropos-do-all"
"apropos-documentation-sort-by-scores"
"apropos-match-face"
"apropos-sort-by-scores"
"async-shell-command-buffer"
"auth-source-cache-expiry"
"auto-coding-alist"
"auto-coding-functions"
"auto-coding-regexp-alist"
"auto-compression-mode"
"auto-encryption-mode"
"auto-fill-inhibit-regexp"
"auto-hscroll-mode"
"auto-image-file-mode"
"auto-insert-mode"
"auto-mode-alist"
"auto-mode-case-fold"
"auto-save-default"
"auto-save-file-name-transforms"
"auto-save-interval"
"auto-save-list-file-prefix"
"auto-save-timeout"
"auto-save-visited-file-name"
"autoarg-kp-mode"
"autoarg-mode"
"automatic-hscrolling"
"automount-dir-prefix"
"backup-by-copying"
"backup-by-copying-when-linked"
"backup-by-copying-when-mismatch"
"backup-by-copying-when-privileged-mismatch"
"backup-directory-alist"
"backward-delete-char-untabify-method"
"bahai-holidays"
"baud-rate"
"bdf-directory-list"
"before-save-hook"
"bidi-paragraph-direction"
"blink-cursor"
"blink-cursor-alist"
"blink-cursor-delay"
"blink-cursor-interval"
"blink-cursor-mode"
"blink-matching-delay"
"blink-matching-paren"
"blink-matching-paren-distance"
"blink-matching-paren-dont-ignore-comments"
"blink-matching-paren-on-screen"
"break-hardlink-on-save"
"browse-url-browser-function"
"buffer-offer-save"
"buffers-menu-buffer-name-length"
"buffers-menu-max-size"
"buffers-menu-show-directories"
"buffers-menu-show-status"
"case-fold-search"
"case-replace"
"change-major-mode-with-file-name"
"charset-map-path"
"christian-holidays"
"colon-double-space"
"column-number-mode"
"comment-auto-fill-only-comments"
"comment-column"
"comment-empty-lines"
"comment-fill-column"
"comment-inline-offset"
"comment-multi-line"
"comment-padding"
"comment-style"
"compilation-ask-about-save"
"compilation-disable-input"
"compilation-mode-hook"
"compilation-search-path"
"compilation-start-hook"
"compilation-window-height"
"compile-command"
"completion-auto-help"
"completion-category-overrides"
"completion-cycle-threshold"
"completion-ignored-extensions"
"completion-in-region-mode"
"completion-pcm-complete-word-inserts-delimiters"
"completion-pcm-word-delimiters"
"completion-show-help"
"completion-styles"
"completions-format"
"compose-mail-user-agent-warnings"
"confirm-kill-emacs"
"confirm-nonexistent-file-or-buffer"
"create-lockfiles"
"crisp-mode"
"ctl-arrow"
"cua-mode"
"current-language-environment"
"cursor-in-non-selected-windows"
"custom-browse-sort-alphabetically"
"custom-buffer-sort-alphabetically"
"custom-enabled-themes"
"custom-file"
"custom-menu-sort-alphabetically"
"custom-safe-themes"
"custom-theme-directory"
"custom-theme-load-path"
"cvs-dired-action"
"cvs-dired-use-hook"
"debug-ignored-errors"
"debug-on-error"
"debug-on-event"
"debug-on-quit"
"debug-on-signal"
"default-directory"
"default-frame-alist"
"default-input-method"
"default-justification"
"defun-prompt-regexp"
"delete-active-region"
"delete-auto-save-files"
"delete-by-moving-to-trash"
"delete-exited-processes"
"delete-old-versions"
"delete-selection-mode"
"delete-trailing-lines"
"desktop-locals-to-save"
"desktop-save-mode"
"diff-command"
"diff-switches"
"directory-abbrev-alist"
"directory-free-space-args"
"directory-free-space-program"
"dired-kept-versions"
"dired-listing-switches"
"display-battery-mode"
"display-buffer-alist"
"display-buffer-base-action"
"display-buffer-function"
"display-buffer-reuse-frames"
"display-hourglass"
"display-mm-dimensions-alist"
"display-time-day-and-date"
"display-time-mode"
"dnd-open-file-other-window"
"dnd-open-remote-file-function"
"dnd-protocol-alist"
"double-click-fuzz"
"double-click-time"
"dynamic-completion-mode"
"echo-keystrokes"
"edebug-all-defs"
"edebug-all-forms"
"eldoc-minor-mode-string"
"emacs-lisp-docstring-fill-column"
"emacs-lisp-mode-hook"
"emacs-major-version"
"emacs-minor-version"
"enable-kinsoku"
"enable-local-eval"
"enable-local-variables"
"enable-recursive-minibuffers"
"enable-remote-dir-locals"
"eol-mnemonic-dos"
"eol-mnemonic-mac"
"eol-mnemonic-undecided"
"eol-mnemonic-unix"
"epa-file-inhibit-auto-save"
"epa-file-name-regexp"
"epa-global-mail-mode"
"erc-track-minor-mode"
"eval-expression-debug-on-error"
"eval-expression-print-length"
"eval-expression-print-level"
"even-window-heights"
"exec-path"
"exec-suffixes"
"exit-language-environment-hook"
"face-font-family-alternatives"
"face-font-registry-alternatives"
"face-font-selection-order"
"face-x-resources"
"facemenu-add-face-function"
"facemenu-end-add-face"
"facemenu-keybindings"
"facemenu-listed-faces"
"facemenu-new-faces-at-end"
"facemenu-remove-face-function"
"fancy-splash-image"
"ff-special-constructs"
"file-coding-system-alist"
"file-name-at-point-functions"
"file-name-shadow-mode"
"file-name-shadow-properties"
"file-name-shadow-tty-properties"
"file-precious-flag"
"fill-column"
"fill-individual-varying-indent"
"fill-nobreak-invisible"
"fill-nobreak-predicate"
"fill-prefix"
"find-directory-functions"
"find-file-existing-other-name"
"find-file-hook"
"find-file-hooks"
"find-file-run-dired"
"find-file-suppress-same-file-warnings"
"find-file-visit-truename"
"find-file-wildcards"
"find-tag-default-function"
"find-tag-hook"
"fit-frame-to-buffer"
"fit-frame-to-buffer-bottom-margin"
"focus-follows-mouse"
"font-list-limit"
"font-lock-defaults"
"font-lock-global-modes"
"font-lock-maximum-decoration"
"font-lock-maximum-size"
"font-lock-support-mode"
"font-lock-verbose"
"font-use-system-font"
"frame-auto-hide-function"
"frame-background-mode"
"fringe-mode"
"garbage-collection-messages"
"gc-cons-percentage"
"gc-cons-threshold"
"gdb-enable-debug"
"general-holidays"
"global-auto-revert-mode"
"global-cwarn-mode"
"global-ede-mode"
"global-font-lock-mode"
"global-hi-lock-mode"
"global-highlight-changes-mode"
"global-hl-line-mode"
"global-linum-mode"
"global-mark-ring-max"
"global-reveal-mode"
"global-subword-mode"
"global-visual-line-mode"
"global-whitespace-mode"
"global-whitespace-newline-mode"
"glyphless-char-display-control"
"gnus-select-method"
"gnutls-min-prime-bits"
"goal-column"
"gpm-mouse-mode"
"grep-command"
"grep-find-command"
"grep-setup-hook"
"grep-window-height"
"gud-tooltip-mode"
"hebrew-holidays"
"help-at-pt-display-when-idle"
"help-char"
"help-enable-auto-load"
"help-event-list"
"help-mode-hook"
"help-window-select"
"highlight-nonselected-windows"
"hippie-expand-try-functions-list"
"history-delete-duplicates"
"history-length"
"holiday-bahai-holidays"
"holiday-christian-holidays"
"holiday-general-holidays"
"holiday-hebrew-holidays"
"holiday-islamic-holidays"
"holiday-local-holidays"
"holiday-oriental-holidays"
"holiday-other-holidays"
"holiday-solar-holidays"
"hourglass-delay"
"hscroll-margin"
"hscroll-step"
"icomplete-mode"
"icon-map-list"
"idle-update-delay"
"ido-mode"
"image-file-name-extensions"
"image-file-name-regexps"
"image-load-path"
"imagemagick-enabled-types"
"imagemagick-types-inhibit"
"imenu-sort-function"
"indent-tabs-mode"
"indicate-buffer-boundaries"
"indicate-empty-lines"
"indicate-unused-lines"
"inhibit-default-init"
"inhibit-eol-conversion"
"inhibit-local-menu-bar-menus"
"inhibit-splash-screen"
"inhibit-startup-buffer-menu"
"inhibit-startup-echo-area-message"
"inhibit-startup-message"
"inhibit-startup-screen"
"initial-buffer-choice"
"initial-frame-alist"
"initial-major-mode"
"initial-scratch-message"
"input-method-activate-hook"
"input-method-after-insert-chunk-hook"
"input-method-deactivate-hook"
"input-method-highlight-flag"
"input-method-inactivate-hook"
"input-method-use-echo-area"
"input-method-verbose-flag"
"insert-default-directory"
"inverse-video"
"isearch-allow-scroll"
"isearch-hide-immediately"
"isearch-lazy-highlight"
"isearch-lazy-highlight-cleanup"
"isearch-lazy-highlight-initial-delay"
"isearch-lazy-highlight-interval"
"isearch-lazy-highlight-max-at-a-time"
"isearch-resume-in-command-history"
"islamic-holidays"
"ispell-personal-dictionary"
"iswitchb-mode"
"jit-lock-chunk-size"
"jit-lock-context-time"
"jit-lock-contextually"
"jit-lock-defer-contextually"
"jit-lock-defer-time"
"jit-lock-stealth-load"
"jit-lock-stealth-nice"
"jit-lock-stealth-time"
"jit-lock-stealth-verbose"
"jka-compr-compression-info-list"
"jka-compr-load-suffixes"
"jka-compr-mode-alist-additions"
"jka-compr-verbose"
"kept-new-versions"
"kept-old-versions"
"keyboard-coding-system"
"keypad-numlock-setup"
"keypad-numlock-shifted-setup"
"keypad-setup"
"keypad-shifted-setup"
"kill-do-not-save-duplicates"
"kill-read-only-ok"
"kill-ring-max"
"kill-whole-line"
"language-info-custom-alist"
"large-file-warning-threshold"
"last-command"
"latex-block-names"
"latex-inputenc-coding-alist"
"latex-run-command"
"latin1-display"
"latin1-display-ucs-per-lynx"
"lazy-highlight-cleanup"
"lazy-highlight-initial-delay"
"lazy-highlight-interval"
"lazy-highlight-max-at-a-time"
"left-margin"
"line-move-ignore-invisible"
"line-move-visual"
"line-number-display-limit"
"line-number-display-limit-width"
"line-number-mode"
"line-spacing"
"lisp-body-indent"
"lisp-indent-function"
"lisp-indent-offset"
"lisp-interaction-mode-hook"
"lisp-mode-hook"
"list-colors-sort"
"list-directory-brief-switches"
"list-directory-verbose-switches"
"list-matching-lines-buffer-name-face"
"list-matching-lines-default-context-lines"
"list-matching-lines-face"

))

(defvar xah-elisp-elisp-vars-2 nil "List elisp variables names")
(setq xah-elisp-elisp-vars-2 '(

"multibyte-syntax-as-symbol"

"font-lock-builtin-face"
"font-lock-comment-delimiter-face"
"font-lock-comment-face"
"font-lock-constant-face"
"font-lock-doc-face"
"font-lock-function-name-face"
"font-lock-keyword-face"
"font-lock-negation-char-face"
"font-lock-preprocessor-face"
"font-lock-reference-face"
"font-lock-string-face"
"font-lock-type-face"
"font-lock-variable-name-face"
"font-lock-warning-face"
"local-holidays"
"locate-ls-subdir-switches"
"lpr-command"
"lpr-switches"
"ls-lisp-support-shell-wildcards"
"mail-abbrevs-mode"
"mail-archive-file-name"
"mail-citation-hook"
"mail-citation-prefix-regexp"
"mail-complete-style"
"mail-default-directory"
"mail-default-headers"
"mail-default-reply-to"
"mail-dont-reply-to-names"
"mail-from-style"
"mail-header-separator"
"mail-hist-keep-history"
"mail-host-address"
"mail-indentation-spaces"
"mail-interactive"
"mail-mailing-lists"
"mail-personal-alias-file"
"mail-self-blind"
"mail-setup-hook"
"mail-signature"
"mail-signature-file"
"mail-specify-envelope-from"
"mail-use-rfc822"
"mail-user-agent"
"mail-yank-prefix"
"major-mode"
"make-backup-file-name-function"
"make-backup-files"
"make-cursor-line-fully-visible"
"make-pointer-invisible"
"mark-active"
"mark-even-if-inactive"
"mark-ring-max"
"max-lisp-eval-depth"
"max-mini-window-height"
"max-specpdl-size"
"menu-bar-mode"
"menu-prompting"
"message-log-max"
"messages-buffer-max-lines"
"meta-prefix-char"
"minibuffer-auto-raise"
"minibuffer-depth-indicate-mode"
"minibuffer-electric-default-mode"
"minibuffer-frame-alist"
"minibuffer-history-case-insensitive-variables"
"minibuffer-prompt-properties"
"mode-line-default-help-echo"
"mode-line-format"
"mode-line-in-non-selected-windows"
"mode-name"
"mode-require-final-newline"
"mouse-1-click-follows-link"
"mouse-1-click-in-non-selected-windows"
"mouse-autoselect-window"
"mouse-avoidance-mode"
"mouse-buffer-menu-maxlen"
"mouse-buffer-menu-mode-mult"
"mouse-drag-copy-region"
"mouse-highlight"
"mouse-scroll-delay"
"mouse-scroll-min-lines"
"mouse-wheel-click-event"
"mouse-wheel-down-event"
"mouse-wheel-follow-mouse"
"mouse-wheel-inhibit-click-time"
"mouse-wheel-mode"
"mouse-wheel-progressive-speed"
"mouse-wheel-scroll-amount"
"mouse-wheel-up-event"
"mouse-yank-at-point"
"msb-mode"
"next-error-highlight"
"next-error-highlight-no-select"
"next-error-hook"
"next-error-recenter"
"next-line-add-newlines"
"next-screen-context-lines"
"no-redraw-on-reenter"
"normal-erase-is-backspace"
"occur-excluded-properties"
"occur-hook"
"occur-mode-find-occurrence-hook"
"occur-mode-hook"
"only-global-abbrevs"
"open-paren-in-column-0-is-defun-start"
"oriental-holidays"
"other-holidays"
"overflow-newline-into-fringe"
"overline-margin"
"package-enable-at-startup"
"page-delimiter"
"paragraph-ignore-fill-prefix"
"paragraph-separate"
"paragraph-start"
"parens-require-spaces"
"parse-sexp-ignore-comments"
"parse-sexp-lookup-properties"
"password-cache"
"password-cache-expiry"
"polling-period"
"pop-up-frame-alist"
"pop-up-frame-function"
"pop-up-frames"
"pop-up-windows"
"pre-abbrev-expand-hook"
"printer-name"
"process-connection-type"
"ps-page-dimensions-database"
"ps-paper-type"
"ps-print-color-p"
"query-replace-from-history-variable"
"query-replace-highlight"
"query-replace-lazy-highlight"
"query-replace-show-replacement"
"query-replace-skip-read-only"
"query-replace-to-history-variable"
"rcirc-track-minor-mode"
"read-buffer-completion-ignore-case"
"read-buffer-function"
"read-file-name-completion-ignore-case"
"read-mail-command"
"read-quoted-char-radix"
"recenter-positions"
"recenter-redisplay"
"recentf-mode"
"regexp-search-ring-max"
"register-separator"
"remote-file-name-inhibit-cache"
"remote-shell-program"
"replace-lax-whitespace"
"replace-regexp-lax-whitespace"
"require-final-newline"
"revert-without-query"
"rmail-displayed-headers"
"rmail-dont-reply-to-names"
"rmail-file-name"
"rmail-highlighted-headers"
"rmail-ignored-headers"
"rmail-primary-inbox-list"
"rmail-retry-ignored-headers"
"rmail-secondary-file-directory"
"rmail-secondary-file-regexp"
"rmail-show-message-hook"
"rmail-spool-directory"
"rmail-user-mail-address-regexp"
"safe-local-eval-forms"
"safe-local-variable-values"
"same-window-buffer-names"
"same-window-regexps"
"save-abbrevs"
"save-interprogram-paste-before-kill"
"savehist-mode"
"scalable-fonts-allowed"
"scroll-all-mode"
"scroll-bar-mode"
"scroll-conservatively"
"scroll-down-aggressively"
"scroll-error-top-bottom"
"scroll-margin"
"scroll-preserve-screen-position"
"scroll-step"
"scroll-up-aggressively"
"search-exit-option"
"search-highlight"
"search-invisible"
"search-nonincremental-instead"
"search-ring-max"
"search-ring-update"
"search-slow-speed"
"search-slow-window-lines"
"search-upper-case"
"search-whitespace-regexp"
"select-active-regions"
"selection-coding-system"
"selective-display-ellipses"
"semantic-default-submodes"
"semantic-mode"
"send-mail-function"
"sentence-end"
"sentence-end-base"
"sentence-end-double-space"
"sentence-end-without-period"
"sentence-end-without-space"
"server-mode"
"set-language-environment-hook"
"set-mark-command-repeat-pop"
"set-mark-default-inactive"
"shell-dumb-shell-regexp"
"shell-file-name"
"shift-select-mode"
"show-paren-mode"
"show-trailing-whitespace"
"site-run-file"
"size-indication-mode"
"slitex-run-command"
"small-temporary-file-directory"
"solar-holidays"
"special-display-buffer-names"
"special-display-frame-alist"
"special-display-function"
"special-display-regexps"
"split-height-threshold"
"split-width-threshold"
"split-window-keep-point"
"split-window-preferred-function"
"standard-indent"
"strokes-mode"
"suggest-key-bindings"
"switch-to-buffer-preserve-window-point"
"switch-to-visible-buffer"
"system-type"
"tab-always-indent"
"tab-stop-list"
"tab-width"
"table-cell-map-hook"
"table-load-hook"
"table-point-entered-cell-hook"
"table-point-left-cell-hook"
"tags-add-tables"
"tags-case-fold-search"
"tags-compression-info-list"
"tags-table-list"
"temp-buffer-max-height"
"temp-buffer-resize-mode"
"temp-buffer-show-function"
"temporary-file-directory"
"term-file-prefix"
"texinfo-close-quote"
"texinfo-open-quote"
"text-mode-hook"
"this-command"
"three-step-help"
"timer-max-repeats"
"tool-bar-max-label-size"
"tool-bar-mode"
"tool-bar-position"
"tool-bar-style"
"tooltip-delay"
"tooltip-frame-parameters"
"tooltip-hide-delay"
"tooltip-mode"
"tooltip-recent-seconds"
"tooltip-short-delay"
"tooltip-use-echo-area"
"tooltip-x-offset"
"tooltip-y-offset"
"tpu-edt-mode"
"trace-buffer"
"track-eol"
"tramp-mode"
"tramp-syntax"
"transient-mark-mode"
"trash-directory"
"truncate-lines"
"truncate-partial-width-windows"
"tutorial-directory"
"type-break-mode"
"underline-minimum-offset"
"undo-ask-before-discard"
"undo-limit"
"undo-outer-limit"
"undo-strong-limit"
"unibyte-display-via-language-environment"
"unify-8859-on-decoding-mode"
"unify-8859-on-encoding-mode"
"url-debug"
"url-handler-mode"
"use-dialog-box"
"use-empty-active-region"
"use-file-dialog"
"user-full-name"
"user-mail-address"
"vc-before-checkin-hook"
"vc-checkin-hook"
"vc-checkout-hook"
"vc-consult-headers"
"vc-directory-exclusion-list"
"vc-display-status"
"vc-follow-symlinks"
"vc-handled-backends"
"vc-ignore-dir-regexp"
"vc-keep-workfiles"
"vc-make-backup-files"
"vc-mistrust-permissions"
"vc-rcs-master-templates"
"vc-sccs-master-templates"
"vc-stay-local"
"version-control"
"vertical-centering-font-regexp"
"view-read-only"
"view-remove-frame-by-deleting"
"visible-bell"
"visible-cursor"
"visual-line-fringe-indicators"
"void-text-area-pointer"
"which-function-mode"
"window-combination-limit"
"window-combination-resize"
"window-min-height"
"window-min-width"
"window-sides-slots"
"window-sides-vertical"
"winner-mode"
"woman-locale"
"word-wrap"
"words-include-escapes"
"x-bitmap-file-path"
"x-dnd-known-types"
"x-dnd-test-function"
"x-dnd-types-alist"
"x-gtk-file-dialog-help-text"
"x-gtk-show-hidden-files"
"x-gtk-stock-map"
"x-gtk-use-old-file-dialog"
"x-gtk-use-system-tooltips"
"x-gtk-whole-detached-tool-bar"
"x-select-enable-clipboard"
"x-select-enable-clipboard-manager"
"x-select-enable-primary"
"x-select-request-type"
"x-stretch-cursor"
"x-underline-at-descent-line"
"x-use-underline-position-properties"
"xterm-mouse-mode"
"yank-excluded-properties"
"yank-handled-properties"
"yank-menu-length"
"yank-pop-change-selection"
"user-emacs-directory"

"buffer-invisibility-spec"

))

(defvar xah-elisp-elisp-all-keywords nil "list of all elisp keywords")
(setq xah-elisp-elisp-all-keywords (append xah-elisp-elisp-lang-words xah-elisp-emacs-words xah-elisp-emacs-user-commands xah-elisp-keyword-builtin xah-elisp-elisp-vars-1 xah-elisp-elisp-vars-2))



;; emacs 24.4 or 24.3 change fix

(defun xah-elisp-up-list (arg1 &optional arg2 arg3)
  "Backward compatibility fix for emacs 24.4's up-list.
emacs 24.4 changed up-list to take up to 3 args. Before, only 1.
See
 `backward-up-list',
 `up-list'"
  (interactive)
  (if (>= emacs-major-version 25)
      (up-list arg1 arg2 arg3)
    (up-list arg1)))


;; completion

(defun xah-elisp-complete-symbol ()
  "Perform keyword completion on current word.
This uses `ido-mode' user interface for completion."
  (interactive)
  (let* (
         (ξbds (bounds-of-thing-at-point 'symbol))
         (ξp1 (car ξbds))
         (ξp2 (cdr ξbds))
         (ξcurrent-sym
          (if  (or (null ξp1) (null ξp2) (equal ξp1 ξp2))
              ""
            (buffer-substring-no-properties ξp1 ξp2)))
         ξresult-sym)
    (when (not ξcurrent-sym) (setq ξcurrent-sym ""))
    (setq ξresult-sym
          (ido-completing-read "" xah-elisp-elisp-all-keywords nil nil ξcurrent-sym ))
    (delete-region ξp1 ξp2)
    (insert ξresult-sym)

    ;; use case of completion

    (when (not (xah-elisp-start-with-left-paren-p))
      (let ( (ξabbrev-expanded-p (xah-elisp-expand-abbrev)))
        ;; (when (not (xah-elisp-start-with-left-paren-p)) (xah-elisp-add-paren-around-symbol))
))))

(defun xah-elisp-start-with-left-paren-p ()
  "true or false"
  (interactive)
  (save-excursion
    (forward-symbol -1) (backward-char 1)
    (if (looking-at "(")
      t
      nil)))

(defun xah-elisp-add-paren-around-symbol ()
  "add paren around symbol before cursor and add a space before closing paren, place cursor ther.
 ⁖
 do-something▮
becomes
 (do-something ▮)
"
  (interactive)
  (forward-symbol -1) (insert "(") (forward-symbol 1) (insert " )")
  (backward-char 1))

(defun xah-elisp-remove-paren-pair ()
  "Remove closest outer paren around cursor or remove string quote and activate the region.
Cursor is moved to the left deleted paren spot, mark is set to the right deleted paren spot.
Call `exchange-point-and-mark' to highlight them.
“closest outer paren” is based on left side of cursor.
"
  (interactive)
  (let ((pos (point))
        p1 p2
        )
    (atomic-change-group
      (xah-elisp-up-list -1 "ESCAPE-STRINGS" "NO-SYNTAX-CROSSING")
      (while (not (char-equal (char-after) ?\( ))
        (xah-elisp-up-list -1 "ESCAPE-STRINGS" "NO-SYNTAX-CROSSING"))
      (setq p1 (point))
      (forward-sexp)
      (setq p2 (point))
      (delete-char -1)
      (push-mark (point) t t)
      (goto-char p1)
      (delete-char 1))))

(defun xah-elisp-expand-abbrev-maybe (&optional φexpand-func)
  "Expand emacs lisp function name before cursor into template.
Returns true if there's a expansion, else false."
  (interactive)
  (let (
        ξp1 ξp2
        ξab-str
        (ξsyntax-state (syntax-ppss)))
    (if (or (nth 3 ξsyntax-state) (nth 4 ξsyntax-state))
        nil
      (xah-elisp-expand-abbrev))
    ;; (xah-elisp-expand-abbrev)
    ))

(put 'xah-elisp-expand-abbrev-maybe 'no-self-insert t)

(defun xah-elisp-expand-abbrev ()
  "Expand the symbol before cursor.
Returns true if there's a expansion, else false."
  (interactive)
  (let (
        ξp1 ξp2
        ξab-str
        )
    (save-excursion
      (forward-symbol -1)
      (setq ξp1 (point))
      (forward-symbol 1)
      (setq ξp2 (point)))
    (setq ξab-str (buffer-substring-no-properties ξp1 ξp2))
    (if (abbrev-symbol ξab-str)
        (progn
          (abbrev-insert (abbrev-symbol ξab-str) ξab-str ξp1 ξp2 )
          (xah-elisp--abbrev-position-cursor ξp1)
          t)
      nil)))

(defun xah-elisp-abbrev-enable-function ()
  "Determine whether to expand abbrev.
This is called by emacs abbrev system."
  (let ((ξsyntax-state (syntax-ppss)))
    (if (or (nth 3 ξsyntax-state) (nth 4 ξsyntax-state))
        nil
      t)))

(defun xah-elisp--abbrev-position-cursor (&optional φpos)
  "Move cursor back to ▮.
but limit backward search to at φpos or at beginning of line.
return true if found, else false."
  (interactive)
  (search-backward "▮" (if φpos φpos (line-beginning-position)) t ))


;; indent/reformat related

(defun xah-elisp-complete-or-indent ()
  "Do keyword completion or indent/prettify-format.

If char before point is letters and char after point is whitespace or punctuation, then do completion, except when in string or comment. In these cases, do `xah-elisp-prettify-root-sexp'."
  (interactive)
  ;; consider the char to the left or right of cursor. Each side is either empty or char.
  ;; there are 4 cases:
  ;; space▮space → do indent
  ;; space▮char → do indent
  ;; char▮space → do completion
  ;; char ▮char → do indent
  (let ( (ξsyntax-state (syntax-ppss)))
    (if (or (nth 3 ξsyntax-state) (nth 4 ξsyntax-state))
        (progn
          (xah-elisp-prettify-root-sexp))
      (progn (if
                 (and (looking-back "[-_a-zA-Z]")
                      (or (eobp) (looking-at "[\n[:blank:][:punct:]]")))
                 (xah-elisp-complete-symbol)
               (xah-elisp-prettify-root-sexp))))))

(defun xah-elisp-prettify-root-sexp ()
  "Prettify format current root sexp group.
Root sexp group is the outmost sexp unit."
  (interactive)
  (save-excursion
    (let (ξp1 ξp2)
      (xah-elisp-goto-outmost-bracket)
      (setq ξp1 (point))
      (setq ξp2 (scan-sexps (point) 1))
      (progn
        (goto-char ξp1)
        (indent-sexp)
        (xah-elisp-compact-parens-region ξp1 ξp2)))))

(defun xah-elisp-goto-outmost-bracket (&optional φpos)
  "Move cursor to the beginning of outer-most bracket, with respect to φpos.
Returns true if point is moved, else false."
  (interactive)
  (let ((ξi 0)
        (ξp0 (if (number-or-marker-p φpos)
                 φpos
               (point))))
    (goto-char ξp0)
    (while
        (and (< (setq ξi (1+ ξi)) 20)
             (not (eq (nth 0 (syntax-ppss (point))) 0)))
      (xah-elisp-up-list -1 "ESCAPE-STRINGS" "NO-SYNTAX-CROSSING"))
    (if (equal ξp0 (point))
        nil
      t
      )))

(defun xah-elisp-compact-parens (&optional φbegin φend)
  "Remove whitespaces in ending repetition of parenthesises.
If there's a text selection, act on the region, else, on defun block."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (save-excursion
       (xah-elisp-goto-outmost-bracket)
       (list (point) (scan-sexps (point) 1)))))
  (let ((ξp1 φbegin) (ξp2 φend))
    (when (null φbegin)
      (save-excursion
        (xah-elisp-goto-outmost-bracket)
        (setq ξp1 (point))
        (setq ξp2 (scan-sexps (point) 1))))
    (xah-elisp-compact-parens-region ξp1 ξp2)))

(defun xah-elisp-compact-parens-region (φbegin φend)
  "Remove whitespaces in ending repetition of parenthesises in region."
  (interactive "r")
  (let (ξsyntax-state)
    (save-restriction
      (narrow-to-region φbegin φend)
      (goto-char (point-min))
      (while (search-forward-regexp ")[ \t\n]+)" nil t)
        (setq ξsyntax-state (syntax-ppss (match-beginning 0)))
        (if (or (nth 3 ξsyntax-state ) (nth 4 ξsyntax-state))
            (progn (search-forward ")"))
          (progn (replace-match "))")
                 (search-backward ")")))))))


;; abbrev

(setq xah-elisp-abbrev-table nil)

(define-abbrev-table 'xah-elisp-abbrev-table
  '(
    ("3t" "(when ▮)" nil :system t) ;test

    ("d" "(defun ▮ ()
  \"DOCSTRING\"
  (interactive)
  (let (VAR)

  ))" nil :system t)
    ("i" "(insert ▮)" nil :system t)
    ("l" "(let (▮)
 x
)" nil :system t)
    ("m" "(message \"%s▮\" ARGS)" nil :system t)
    ("p" "(point)" nil :system t)
    ("s" "(setq ▮)" nil :system t)

    ("ah" "add-hook" nil :system t)
    ("bc" "backward-char" nil :system t)
    ("bfn" "buffer-file-name" nil :system t)
    ("bmp" "buffer-modified-p" nil :system t)
    ("bol" "beginning-of-line" nil :system t)
    ("botap" "bounds-of-thing-at-point" nil :system t)
    ("bs" "buffer-substring" nil :system t)
    ("bsnp" "buffer-substring-no-properties" nil :system t)
    ("ca" "custom-autoload" nil :system t)
    ("cb" "current-buffer" nil :system t)
    ("cc" "condition-case" nil :system t)
    ("cd" "copy-directory" nil :system t)
    ("cdr" "cdr" nil :system t)
    ("cf" "copy-file" nil :system t)
    ("dc" "delete-char" nil :system t)
    ("dd" "delete-directory" nil :system t)
    ("df" "delete-file" nil :system t)
    ("dk" "define-key" nil :system t)
    ("dr" "delete-region" nil :system t)
    ("efn" "expand-file-name" nil :system t)
    ("eol" "end-of-line" nil :system t)
    ("fc" "forward-char" nil :system t)
    ("ff" "find-file" nil :system t)
    ("fl" "forward-line" nil :system t)
    ("fnd" "file-name-directory" nil :system t)
    ("fne" "file-name-extension" nil :system t)
    ("fnn" "file-name-nondirectory" nil :system t)
    ("fnse" "file-name-sans-extension" nil :system t)
    ("frn" "file-relative-name" nil :system t)
    ("gc" "goto-char" nil :system t)
    ("gnb" "generate-new-buffer" nil :system t)
    ("gsk" "global-set-key" nil :system t)
    ("ifc" "insert-file-contents" nil :system t)
    ("kb" "kill-buffer" nil :system t)
    ("la" "looking-at" nil :system t)
    ("lbp" "line-beginning-position" nil :system t)
    ("lep" "line-end-position" nil :system t)
    ("mb" "match-beginning" nil :system t)
    ("md" "make-directory" nil :system t)
    ("me" "match-end" nil :system t)
    ("mlv" "make-local-variable" nil :system t)
    ("ms" "match-string" nil :system t)
    ("nts" "number-to-string" nil :system t)
    ("ntr" "narrow-to-region" nil :system t)
    ("pmi" "point-min" nil :system t)
    ("pm" "point-max" nil :system t)
    ("rap" "region-active-p" nil :system t)
    ("urp" "use-region-p" nil :system t)
    ("rb" "region-beginning" nil :system t)
    ("re" "region-end" nil :system t)
    ("rf" "rename-file" nil :system t)
    ("rm" "replace-match" nil :system t)
    ("rq" "regexp-quote" nil :system t)
    ("rr" "replace-regexp" nil :system t)
    ("rris" "replace-regexp-in-string" nil :system t)
    ("rsb" "re-search-backward" nil :system t)
    ("rsf" "re-search-forward" nil :system t)
    ("sb" "search-backward" nil :system t)
    ("sbr" "search-backward-regexp" nil :system t)
    ("sc" "shell-command" nil :system t)
    ("scb" "skip-chars-backward" nil :system t)
    ("scf" "skip-chars-forward" nil :system t)
    ("se" "save-excursion" nil :system t)
    ("sf" "search-forward" nil :system t)
    ("sfm" "set-file-modes" nil :system t)
    ("sfr" "search-forward-regexp" nil :system t)
    ("sm" "string-match" nil :system t)
    ("sr" "save-restriction" nil :system t)
    ("ss" "split-string" nil :system t)
    ("stn" "string-to-number" nil :system t)
    ("str" "string" nil :system t)
    ("tap" "thing-at-point" nil :system t)
    ("wcb" "with-current-buffer" nil :system t)
    ("wg" "widget-get" nil :system t)
    ("yonp" "yes-or-no-p" nil :system t)

    ("add-hook" "(add-hook 'HOOK▮ 'FUNCTION)" nil :system t)
    ("and" "(and ▮)" nil :system t )
    ("version<" "(version< \"24.4\" emacs-version)" nil :system t )
    ("version<=" "(version<= \"24.4\" emacs-version)" nil :system t )

    ("append" "(append ▮)" nil :system t)
    ("add-to-list" "(add-to-list LIST-VAR▮ ELEMENT &optional APPEND COMPARE-FN)" nil :system t)
    ("apply" "(apply ▮)" nil :system t)
    ("aref" "(aref ARRAY▮ INDEX)" nil :system t)
    ("aset" "(aset ARRAY▮ IDX NEWELT)" nil :system t)
    ("assoc" "(assoc KEY▮ LIST)" nil :system t)
    ("assq" "(assq KEY▮ LIST)" nil :system t)
    ("autoload" "(autoload 'FUNCNAME▮ \"FILENAME\" &optional \"DOCSTRING\" INTERACTIVE TYPE)" nil :system t)
    ("backward-char" "(backward-char ▮)" nil :system t)
    ("beginning-of-line" "(beginning-of-line)" nil :system t)
    ("boundp" "(boundp '▮)" nil :system t)
    ("bounds-of-thing-at-point" "(bounds-of-thing-at-point 'symbol▮ 'filename 'word 'whitespace 'line)")

    ("buffer-file-name" "(buffer-file-name)" nil :system t)
    ("buffer-modified-p" "(buffer-modified-p ▮)" nil :system t)
    ("buffer-substring-no-properties" "(buffer-substring-no-properties START▮ END)" nil :system t)
    ("buffer-substring" "(buffer-substring START▮ END)" nil :system t)
    ("call-interactively" "(call-interactively 'FUNCTION▮ &optional RECORD-FLAG KEYS)" nil :system t)
    ("called-interactively-p" "(called-interactively-p 'interactive▮)" nil :system t)
    ("car" "(car LIST▮)" nil :system t)
    ("catch" "(catch TAG▮ BODY)" nil :system t)
    ("cdr" "(cdr LIST▮)" nil :system t)
    ("concat" "(concat \"▮\" \"▮\")" nil :system t)
    ("cond" "(cond
(CONDITION▮ BODY)
(CONDITION BODY)
)" nil :system t)
    ("condition-case" "(condition-case ▮)" nil :system t)
    ("cons" "(cons CAR▮ CDR)" nil :system t)
    ("consp" "(consp ▮)" nil :system t)
    ("copy-directory" "(copy-directory ▮ NEWNAME &optional KEEP-TIME PARENTS)" nil :system t)
    ("copy-file" "(copy-file FILE▮ NEWNAME &optional OK-IF-ALREADY-EXISTS KEEP-TIME PRESERVE-UID-GID)" nil :system t)
    ("current-buffer" "(current-buffer)" nil :system t)
    ("custom-autoload" "(custom-autoload ▮ SYMBOL LOAD &optional NOSET)" nil :system t)
    ("defalias" "(defalias 'SYMBOL▮ 'DEFINITION &optional DOCSTRING)" nil :system t)
    ("defconst" "(defconst ▮ INITVALUE \"DOCSTRING\")" nil :system t)
    ("defcustom" "(defcustom ▮ VALUE \"DOC\" &optional ARGS)" nil :system t)
    ("define-key" "(define-key KEYMAPNAME▮ (kbd \"M-b\") 'FUNCNAME)" nil :system t)
    ("defsubst" "(defsubst ▮)" nil :system t)
    ("defun" "(defun ▮ ()
  \"DOCSTRING\"
  (interactive)
  (let (VAR)

  ))" nil :system t)
    ("defvar" "(defvar ▮ &optional INITVALUE \"DOCSTRING\")" nil :system t)
    ("delete-char" "(delete-char ▮)" nil :system t)
    ("delete-directory" "(delete-directory ▮ &optional RECURSIVE)" nil :system t)
    ("delete-file" "(delete-file ▮)" nil :system t)
    ("delete-region" "(delete-region pos1▮ pos2)" nil :system t)
    ("directory-files" "(directory-files ▮ &optional FULL MATCH NOSORT)" nil :system t)
    ("dolist" "(dolist (VAR▮ LIST [RESULT]) BODY)" nil :system t)
    ("dotimes" "(dotimes (VAR▮ COUNT [RESULT]) BODY)" nil :system t)
    ("elt" "(elt SEQUENCE▮ N)" nil :system t)
    ("end-of-line" "(end-of-line ▮&optional N)" nil :system t)
    ("eq" "(eq ▮)" nil :system t)
    ("equal" "(equal ▮)" nil :system t)
    ("error" "(error \"%s\" ▮)" nil :system t)
    ("expand-file-name" "(expand-file-name ▮ &optional relativedir)" nil :system t)
    ("format" "(format \"▮\" &optional OBJECTS)" nil :system t)
    ("fboundp" "(fboundp '▮)" nil :system t)
    ("featurep" "(featurep 'FEATURE▮)" nil :system t)
    ("file-directory-p" "(file-directory-p ▮)" nil :system t)
    ("file-exists-p" "(file-exists-p ▮)" nil :system t)
    ("file-name-directory" "(file-name-directory ▮)" nil :system t)
    ("file-name-extension" "(file-name-extension ▮ &optional PERIOD)" nil :system t)
    ("file-name-nondirectory" "(file-name-nondirectory ▮)" nil :system t)
    ("file-name-sans-extension" "(file-name-sans-extension ▮)" nil :system t)
    ("file-regular-p" "(file-regular-p ▮)" nil :system t)
    ("file-relative-name" "(file-relative-name ▮)" nil :system t)
    ("find-file" "(find-file ▮)" nil :system t)
    ("format" "(format \"%s\" ▮)" nil :system t)
    ("forward-char" "(forward-char ▮)" nil :system t)
    ("forward-line" "(forward-line ▮)" nil :system t)
    ("funcall" "(funcall ▮)" nil :system t)
    ("function" "(function ▮)" nil :system t)
    ("generate-new-buffer" "(generate-new-buffer ▮)" nil :system t)
    ("get" "(get SYMBOL▮ PROPNAME)" nil :system t)
    ("global-set-key" "(global-set-key (kbd \"C-▮\") 'COMMAND)" nil :system t)
    ("goto-char" "(goto-char ▮)" nil :system t)
    ("if" "(if ▮
    (progn )
  (progn )
)" nil :system t)
    ("insert-file-contents" "(insert-file-contents ▮ &optional VISIT BEG END REPLACE)" nil :system t)
    ("insert" "(insert ▮)" nil :system t)
    ("interactive" "(interactive)" nil :system t)
    ("kbd" "(kbd \"▮\")" nil :system t)
    ("kill-buffer" "(kill-buffer ▮)" nil :system t)
    ("lambda" "(lambda (▮) BODY)" nil :system t)
    ("length" "(length ▮)" nil :system t)
    ("let" "(let (▮)
 x
)" nil :system t)
    ("line-beginning-position" "(line-beginning-position)" nil :system t)
    ("line-end-position" "(line-end-position)" nil :system t)
    ("load" "(load FILE▮ &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)" nil :system t)
    ("looking-at" "(looking-at \"REGEXP▮\")" nil :system t)
    ("make-directory" "(make-directory ▮ &optional PARENTS)" nil :system t)
    ("make-local-variable" "(make-local-variable ▮)" nil :system t)
    ("make-list" "(make-list LENGTH▮ INIT)" nil :system t)
    ("mapc" "(mapc '▮ SEQUENCE)" nil :system t)
    ("mapcar" "(mapcar '▮ SEQUENCE)" nil :system t)
    ("mapconcat" "(mapconcat FUNCTION▮ SEQUENCE SEPARATOR)" nil :system t)
    ("match-beginning" "(match-beginning N▮)" nil :system t)
    ("match-end" "(match-end N▮)" nil :system t)
    ("match-data" "(match-data &optional INTEGERS▮ REUSE RESEAT)" nil :system t)
    ("match-string" "(match-string NUM▮ &optional STRING)" nil :system t)
    ("member" "(member ELT▮ LIST)" nil :system t)
    ("memq" "(memq ELT▮ LIST)" nil :system t)
    ("not" "(not ▮)" nil :system t)
    ("delq" "(delq ELT▮ LIST)" nil :system t)
    ("delete" "(delete OBJECT▮ SEQUENCE)" nil :system t)
    ("delete-dups" "(delete-dups LIST▮)" nil :system t)

    ("remq" "(remq OBJECT▮ LIST)" nil :system t)
    ("memql" "(memql OBJECT▮ LIST)" nil :system t)
    ("member" "(member OBJECT▮ LIST)" nil :system t)
    ("remove" "(remove OBJECT▮ SEQUENCE)" nil :system t)
    ("member-ignore-case" "(member-ignore-case OBJECT▮ LIST)" nil :system t)

    ("message" "(message \"%s▮\" ARGS)" nil :system t)
    ("narrow-to-region" "(narrow-to-region START▮ END)" nil :system t)
    ("nth" "(nth N▮ LIST)" nil :system t)
    ("null" "(null ▮)" nil :system t)
    ("number-to-string" "(number-to-string ▮)" nil :system t)
    ("number-sequence" "(number-sequence FROM▮ &optional TO INC)" nil :system t)
    ("or" "(or ▮)" nil :system t)
    ("point-max" "(point-max)" nil :system t)
    ("point-min" "(point-min)" nil :system t)
    ("point" "(point)" nil :system t)
    ("prin1" "(prin1 ▮)" nil :system t)
    ("princ" "(princ ▮)" nil :system t)
    ("print" "(print ▮)" nil :system t)
    ("progn" "(progn ▮)" nil :system t)
    ("push" "(push NEWELT▮ PLACE)" nil :system t)
    ("push-mark" "(push-mark ▮&optional LOCATION NOMSG ACTIVATE)" nil :system t)
    ("put" "(put 'SYMBOL▮ PROPNAME VALUE)" nil :system t)
    ("random" "(random ▮)" nil :system t)
    ("rassoc" "(rassoc KEY▮ LIST)" nil :system t)
    ("re-search-backward" "(re-search-backward \"REGEXP▮\" &optional BOUND NOERROR COUNT)" nil :system t)
    ("re-search-forward" "(re-search-forward \"REGEXP▮\" &optional BOUND NOERROR COUNT)" nil :system t)
    ("read-directory-name" "(read-directory-name \"▮\" &optional DIR DEFAULT-DIRNAME MUSTMATCH INITIAL)" nil :system t)
    ("read-file-name" "(read-file-name \"▮\" &optional DIR DEFAULT-FILENAME MUSTMATCH INITIAL PREDICATE)" nil :system t)
    ("read-regexp" "(read-regexp \"▮\" &optional DEFAULT-VALUE)" nil :system t)
    ("read-string" "(read-string \"▮\" &optional INITIAL-INPUT HISTORY DEFAULT-VALUE INHERIT-INPUT-METHOD)" nil :system t)
    ("regexp-opt" "(regexp-opt STRINGS▮ &optional PAREN)" nil :system t)
    ("regexp-quote" "(regexp-quote ▮)" nil :system t)
    ("region-active-p" "(region-active-p)" nil :system t)
    ("region-beginning" "(region-beginning)" nil :system t)
    ("region-end" "(region-end)" nil :system t)
    ("rename-file" "(rename-file FILE▮ NEWNAME &optional OK-IF-ALREADY-EXISTS)" nil :system t)
    ("repeat" "(repeat ▮)" nil :system t)
    ("replace-match" "(replace-match NEWTEXT▮ &optional FIXEDCASE LITERAL \"STRING\" SUBEXP)" nil :system t)
    ("replace-regexp-in-string" "(replace-regexp-in-string \"REGEXP▮\" REP \"STRING\" &optional FIXEDCASE LITERAL SUBEXP START)" nil :system t)
    ("replace-regexp" "(replace-regexp \"REGEXP▮\" TO-STRING &optional DELIMITED START END)" nil :system t)
    ("require" "(require ▮)" nil :system t)
    ("reverse" "(reverse ▮)" nil :system t)
    ("save-buffer" "(save-buffer &optional ARG▮)" nil :system t)
    ("save-excursion" "(save-excursion ▮)" nil :system t)
    ("save-restriction" "(save-restriction ▮)" nil :system t)
    ("search-backward-regexp" "(search-backward-regexp \"▮\" &optional BOUND NOERROR COUNT)" nil :system t)
    ("search-backward" "(search-backward \"▮\" &optional BOUND NOERROR COUNT)" nil :system t)
    ("search-forward-regexp" "(search-forward-regexp \"▮\" &optional BOUND NOERROR COUNT)" nil :system t)
    ("search-forward" "(search-forward \"▮\" &optional BOUND NOERROR COUNT)" nil :system t)
    ("set-buffer" "(set-buffer ▮)" nil :system t)
    ("set-file-modes" "(set-file-modes ▮ MODE)" nil :system t)
    ("set-mark" "(set-mark ▮)" nil :system t)
    ("setq" "(setq ▮)" nil :system t)
    ("shell-command" "(shell-command ▮ &optional OUTPUT-BUFFER ERROR-BUFFER)" nil :system t)
    ("skip-chars-backward" "(skip-chars-backward \"▮\" &optional LIM)" nil :system t)
    ("skip-chars-forward" "(skip-chars-forward \"▮\" &optional LIM)" nil :system t)
    ("split-string" "(split-string ▮ &optional SEPARATORS OMIT-NULLS)" nil :system t)
    ("string" "(string ▮)" nil :system t)
    ("string=" "(string-equal str1▮ str2)" nil :system t)
    ("string-equal" "(string-equal str1▮ str2)" nil :system t)
    ("string-match-p" "(string-match-p \"REGEXP▮\" \"STRING\" &optional START)" nil :system t)
    ("string-match" "(string-match \"REGEXP▮\" \"STRING\" &optional START)" nil :system t)
    ("string-to-number" "(string-to-number \"▮\")" nil :system t)
    ("stringp" "(stringp ▮)" nil :system t)
    ("substring-no-properties" "(substring-no-properties ▮ FROM TO)" nil :system t)
    ("substring" "(substring STRING▮ FROM &optional TO)" nil :system t)
    ("thing-at-point" "(thing-at-point 'symbol▮ 'filename 'word 'whitespace 'line)")
    ("throw" "(throw TAG▮ VALUE)" nil :system t)
    ("unless" "(unless ▮)" nil :system t)
    ("use-region-p" "(use-region-p)" nil :system t)
    ("user-error" "(user-error FORMAT▮ &rest ARGS)" nil :system t)
    ("vector" "(vector ▮)" nil :system t)
    ("when" "(when ▮)" nil :system t)
    ("while" "(while (< ξi▮ 9)
      (setq ξi (1+ ξi)))" nil :system t)
    ("widget-get" "(widget-get ▮)" nil :system t)
    ("with-current-buffer" "(with-current-buffer ▮)" nil :system t)
    ("with-temp-buffer" "(with-temp-buffer ▮)" nil :system t)
    ("with-temp-file" "(with-temp-file FILE▮)" nil :system t)
    ("write-file" "(write-file FILENAME▮ &optional CONFIRM)" nil :system t)
    ("write-region" "(write-region (point-min) (point-max) FILENAME &optional APPEND VISIT LOCKNAME MUSTBENEW)" nil :system t)

;; #name: read lines of a file
;; # --
;; (defun read-lines (filePath)
;;   "Return a list of lines in FILEPATH."
;;   (with-temp-buffer
;;     (insert-file-contents filePath)
;;     (split-string
;;      (buffer-string) "\n" t)) )

;; ;; process all lines
;; (mapc
;;  (lambda (aLine)
;;    (message aLine) ; do your stuff here
;;    )
;;  (read-lines "inputFilePath")
;; )
;;

;; #name: find and replace on region
;; # --
;; (defun replace-html-chars-region (start end)
;;   "Replace “<” to “&lt;” and other chars in HTML.
;; This works on the current region."
;;   (interactive "r")
;;   (save-restriction
;;     (narrow-to-region start end)
;;     (goto-char (point-min))
;;     (while (search-forward "&" nil t) (replace-match "&amp;" nil t))
;;     (goto-char (point-min))
;;     (while (search-forward "<" nil t) (replace-match "&lt;" nil t))
;;     (goto-char (point-min))
;;     (while (search-forward ">" nil t) (replace-match "&gt;" nil t))
;;     )
;;   )

;; # --
;; ;; apply a function to all files in a dir
;; (require 'find-lisp)
;; (mapc 'my-process-file (find-lisp-find-files "~/myweb/" "\\.html$"))

;; #name: Command that works on region or word
;; # --
;; ;; example of a command that works on current word or text selection
;; (defun down-case-word-or-region ()
;;   "Lower case the current word or text selection."
;; (interactive)
;; (let (pos1 pos2 meat)
;;   (if (and transient-mark-mode mark-active)
;;       (setq pos1 (region-beginning)
;;             pos2 (region-end))
;;     (setq pos1 (car (bounds-of-thing-at-point 'symbol))
;;           pos2 (cdr (bounds-of-thing-at-point 'symbol))))

;;   ; now, pos1 and pos2 are the starting and ending positions
;;   ; of the current word, or current text selection if exists

;;   ;; put your code here.
;;   ▮
;;   ;; Some example of things you might want to do
;;   (downcase-region pos1 pos2) ; example of a func that takes region as args
;;   (setq meat (buffer-substring-no-properties pos1 pos2)) ; grab the text.
;;   (delete-region pos1 pos2) ; get rid of it
;;   (insert "newText") ; insert your new text

;;   )
;; )

    ("y-or-n-p" "(y-or-n-p \"PROMPT▮ \")" nil :system t)
    ("yes-or-no-p" "(yes-or-no-p \"PROMPT▮ \")" nil :system t))

  "abbrev table for `xah-elisp-mode'"
;; :regexp "\\_<\\([_-0-9A-Za-z]+\\)"
  :regexp "\\([_-0-9A-Za-z]+\\)"
  :case-fixed t
  :enable-function 'xah-elisp-abbrev-enable-function
  )


;; syntax coloring related

(defface xah-elisp-function-param
  '(
    (t :foreground "black" :background "LightYellow"))
  "face for function parameters."
  :group 'xah-elisp-mode )

(defface xah-elisp-user-variable
  '(
    (t :foreground "magenta"))
  "face for user variables."
  :group 'xah-elisp-mode )

(setq xah-elisp-font-lock-keywords
      (let (
            (emacsWords (regexp-opt xah-elisp-emacs-words 'symbols))
            (emacsUserWords (regexp-opt xah-elisp-emacs-user-commands 'symbols))
            (emacsBuiltins (regexp-opt xah-elisp-keyword-builtin 'symbols))
            (elispLangWords (regexp-opt xah-elisp-elisp-lang-words 'symbols))
            (elispVars1 (regexp-opt xah-elisp-elisp-vars-1 'symbols))
            (elispVars2 (regexp-opt xah-elisp-elisp-vars-2 'symbols))
            (functionParameters "φ[-_?0-9A-Za-z]+" )
            (userVars "ξ[-_?0-9A-Za-z]+" ))
        `(
          (,emacsWords . font-lock-function-name-face)
          (,emacsUserWords . font-lock-type-face)
          (,emacsBuiltins . font-lock-builtin-face)
          (,elispLangWords . font-lock-keyword-face)
          (,elispVars1 . font-lock-variable-name-face)
          (,elispVars2 . font-lock-variable-name-face)
          (,functionParameters . 'xah-elisp-function-param)
          (,userVars . 'xah-elisp-user-variable))))

;; font-lock-builtin-face
;; font-lock-comment-delimiter-face
;; font-lock-comment-face
;; font-lock-constant-face
;; font-lock-doc-face
;; font-lock-function-name-face
;; font-lock-keyword-face
;; font-lock-negation-char-face
;; font-lock-preprocessor-face
;; font-lock-reference-face
;; font-lock-string-face
;; font-lock-type-face
;; font-lock-variable-name-face
;; font-lock-warning-face


;; ;; syntax table
;; (defvar xah-elisp-syntax-table nil "Syntax table for `xah-elisp-mode'.")
;; (setq xah-elisp-syntax-table
;;       (let ((synTable (make-syntax-table)))
;;         (modify-syntax-entry ?\; "<" synTable)
;;         (modify-syntax-entry ?\n ">" synTable)
;;         (modify-syntax-entry ?` "'   " synTable)
;;         (modify-syntax-entry ?' "'   " synTable)
;;         (modify-syntax-entry ?, "'   " synTable)
;;         (modify-syntax-entry ?@ "'   " synTable)

;;         synTable))


;; keybinding

(when (string-equal system-type "windows-nt")
  (define-key key-translation-map (kbd "<apps>") (kbd "<menu>")))

(defvar xah-elisp-keymap nil "Keybinding for `xah-elisp-mode'")
(progn
  (setq xah-elisp-keymap (make-sparse-keymap))
  (define-key xah-elisp-keymap (kbd "TAB") 'xah-elisp-complete-or-indent)

  (define-prefix-command 'xah-elisp-single-keys-keymap)

  (define-key xah-elisp-single-keys-keymap (kbd "u") 'xah-elisp-add-paren-around-symbol)

  (define-key xah-elisp-single-keys-keymap (kbd "t") 'xah-elisp-prettify-root-sexp)
  (define-key xah-elisp-single-keys-keymap (kbd "h") 'xah-elisp-remove-paren-pair)

  (define-key xah-elisp-single-keys-keymap (kbd "p") 'xah-elisp-compact-parens)
  (define-key xah-elisp-single-keys-keymap (kbd "c") 'xah-elisp-complete-symbol)

  (define-key xah-elisp-single-keys-keymap (kbd "e") 'xah-elisp-expand-abbrev-maybe))



;; define the mode
(defun xah-elisp-mode ()
  "A major mode for emacs lisp.

Most useful command is `xah-elisp-complete-or-indent'.

Press TAB before word to pretty format (indent).

Press TAB after word to complete.

Press SPACE to expand name to template.

i also recommend you use these commands:
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
or
URL `http://ergoemacs.github.io/ergoemacs-mode/'

\\{xah-elisp-keymap}"
  (interactive)

  (kill-all-local-variables)

  (setq mode-name "∑lisp")
  (setq major-mode 'xah-elisp-mode)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (setq font-lock-defaults '((xah-elisp-font-lock-keywords)))

  (setq local-abbrev-table xah-elisp-abbrev-table)

  (if (or
       (not (boundp 'xfk-major-mode-lead-key))
       (null 'xfk-major-mode-lead-key))
      (define-key xah-elisp-keymap (kbd "<menu> e") xah-elisp-single-keys-keymap)
    (define-key xah-elisp-keymap xfk-major-mode-lead-key xah-elisp-single-keys-keymap))
  (use-local-map xah-elisp-keymap)

  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1) ;default to `;;' in comment-region
  (setq-local comment-column 2)

  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local tab-always-indent 'complete)

  (add-hook 'completion-at-point-functions 'xah-elisp-complete-symbol nil 'local)

  (abbrev-mode 1)

  (progn
    ;; setup auto-complete-mode
    (when (fboundp 'auto-complete-mode)
      (add-to-list 'ac-modes 'xah-elisp-mode)))
  ;; (add-hook 'xah-elisp-mode-hook 'ac-emacs-lisp-mode-setup)

  (make-local-variable abbrev-expand-function)
  (if (or
       (and (>= emacs-major-version 24)
            (>= emacs-minor-version 4))
       (>= emacs-major-version 25))
      (progn
        (setq abbrev-expand-function 'xah-elisp-expand-abbrev-maybe))
    (progn (add-hook 'abbrev-expand-functions 'xah-elisp-expand-abbrev-maybe nil t)))

  (setq prettify-symbols-alist '(("lambda" . 955)))

  (run-mode-hooks 'xah-elisp-mode-hook)

  )

(provide 'xah-elisp-mode)
