;;; xah-elisp-mode.el --- Major mode for editing emacs lisp. -*- coding: utf-8 -*-

;; Copyright © 2013 by Xah Lee

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;; Created: 2013-03-23
;; Keywords: languages, convenience

;; You can redistribute this program and/or modify it. Please give credit and link. Thanks.

;;; Commentary:
;; Major mode for editing emacs lisp. Beta stage.
;; home page: http://ergoemacs.org/emacs/xah-elisp-mode.html

;; version 0.4, 2014-06-22 now features completion and function abbrev/template. (without auto-complete-mode nor yasnippet)
;; version 0.3, 2014-06-10 major kinda rewrite. use at your own risk.
;; version 0.2.1, 2014-04-10 added keyword “remove”
;; version 0.2, 2014-02-18 lots more keywords, and stuff.
;; version 0.1, 2013-03-23 first version

(require 'lisp-mode)

(defvar xah-elisp-mode-hook nil "Standard hook for `xah-elisp-mode'")

(defvar xem-elisp-lang-words nil "a list of elisp keyword more or less related to elisp the language.")
(setq xem-elisp-lang-words '(

"zerop"
"listp"
"numberp"
"functionp"

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
"member"
"memq"
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
"remove"
"car-safe"
) )

(defvar xem-emacs-words nil "a list of keywords more or less related to emacs system.")
(setq xem-emacs-words '(

"frame-parameter"
"frame-parameters"
"modify-frame-parameters"
"set-frame-parameter"
"modify-all-frames-parameters"

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
"forward-sexp"
"generate-new-buffer"
"global-unset-key"
"goto-char"  "insert-file-contents"
"insert"
"interactive"
"kill-region"
"kill-ring-save"
"kill-all-local-variables"
"kill-buffer"
"line-beginning-position"
"line-end-position"
"local-set-key"
"looking-at"
"make-directory"
"make-local-variable"
"mark"
"match-beginning"
"match-end"
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

) )

(defvar xem-emacs-user-commands nil "list of keywords related to user's needs.")
(setq xem-emacs-user-commands '(

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

  )
)

(defvar xem-keyword-builtin nil "a list of elisp names")
(setq xem-keyword-builtin '( "&optional") )

(defvar xem-elisp-vars-1 nil "a list elisp variables names")
(setq xem-elisp-vars-1 '(

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

(defvar xem-elisp-vars-2 nil "a list elisp variables names")
(setq xem-elisp-vars-2 '(

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

) )

(defvar xem-elisp-all-keywords nil "list of all elisp keywords")
(setq xem-elisp-all-keywords (append xem-elisp-lang-words xem-emacs-words xem-emacs-user-commands xem-keyword-builtin xem-elisp-vars-1 xem-elisp-vars-2))


;; syntax coloring related

(setq xem-font-lock-keywords
      (let (
            (emacsWords (regexp-opt xem-emacs-words 'symbols) )
            (emacsUserWords (regexp-opt xem-emacs-user-commands 'symbols) )
            (emacsBuiltins (regexp-opt xem-keyword-builtin 'symbols) )
            (elispLangWords (regexp-opt xem-elisp-lang-words 'symbols) )
            (elispVars1 (regexp-opt xem-elisp-vars-1 'symbols) )
            (elispVars2 (regexp-opt xem-elisp-vars-2 'symbols) )
            )
        `(
          (,emacsWords . font-lock-function-name-face)
          (,emacsUserWords . font-lock-builtin-face)
          (,emacsBuiltins . font-lock-type-face)
          (,elispLangWords . font-lock-keyword-face)
          (,elispVars1 . font-lock-variable-name-face)
          (,elispVars2 . font-lock-variable-name-face)
          ) ) )


;; completion

(defun xem-complete-symbol ()
  "Perform keyword completion on current word.

This uses `ido-mode' user interface style for completion."
  (interactive)
  (let* (

         (bds (bounds-of-thing-at-point 'symbol))
         (p1 (car bds) )
         (p2 (cdr bds) )
         (currentWord (buffer-substring-no-properties p1 p2) )

         finalResult)
    (when (not currentWord) (setq currentWord ""))
    (setq finalResult
          (ido-completing-read "" xem-elisp-all-keywords nil nil currentWord )
          )
    (delete-region p1 p2)
    (insert finalResult)
    ))

(defun xem-complete-or-indent ()
  "Do keyword complete or indent line depending on context.

If there's a text selection, do indent region. Else, if the char
before point is letters and char after point is whitespace or
punctuation, then do completion. Else do indent line."
  (interactive)
  ;; consider the char to the left or right of cursor. Each side is either empty or char.
  ;; there are 4 cases:
  ;; space▮space → do indent
  ;; space▮char → do indent
  ;; char▮space → do completion
  ;; char▮char → do indent
  (if (region-active-p)
      (xem-indent-region (region-beginning) (region-end))
    (if (and
         (looking-at "[\n[:blank:][:punct:]]")
         (looking-back "[-_a-zA-Z]")
         )
        (progn (message "doing complete") ; debug
               (xem-complete-symbol)
               )
      (save-excursion
        (message "doing indent") ; debug
        (xem-goto-root-outer-bracket)
        (indent-sexp (scan-sexps (point) 1) )
        ) ) ) )


;; indent/reformat related

(defun xem-goto-root-outer-bracket ()
  "Move cursor to the beginning of outer-most bracket."
  (interactive)
  (let ((i 0))
      (while
          (and (not (eq (nth 0 (syntax-ppss (point))) 0) )
               (< i 20)
 )
        (setq i (1+ i))
(up-list -1 "ESCAPE-STRINGS" "NO-SYNTAX-CROSSING")
))
  )

(defun xem-indent-line ()
  "Indent lines from parent bracket to matching bracket."
  (interactive)
  (save-excursion
    (backward-up-list)
    (indent-sexp)
    )
  )

(defun xem-indent-region (p1 p2)
  "Indent region."
  (interactive "r")
  (let (p3 p4)
    (save-excursion
      (goto-char p1)
      (indent-sexp p2)
      )
    ) )

(defun xem-compact-parens ()
  "Remove whitespaces in ending repetition of parenthesises.
If there's a text selection, act on the region, else, on defun block.
Warning: This command does not preserve texts inside double quotes (strings) or in comments."
  (interactive)
  (let (p1 p2)
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (save-excursion
        (beginning-of-defun)
        (setq p1 (point))
        (end-of-defun)
        (setq p2 (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)
        (goto-char (point-min))
        (while (search-forward-regexp ")[ \t\n]+)" nil t) (replace-match "))"))
        (goto-char (point-min))
        (while (search-forward-regexp ")[ \t\n]+)" nil t) (replace-match "))"))))))


;; ;; syntax table
;; (defvar xem-syntax-table nil "Syntax table for `xah-elisp-mode'.")
;; (setq xem-syntax-table
;;       (let ((synTable (make-syntax-table)))
;;         (modify-syntax-entry ?\; "<" synTable)
;;         (modify-syntax-entry ?\n ">" synTable)
;;         (modify-syntax-entry ?` "'   " synTable)
;;         (modify-syntax-entry ?' "'   " synTable)
;;         (modify-syntax-entry ?, "'   " synTable)
;;         (modify-syntax-entry ?@ "'   " synTable)

;;         synTable))


;; keybinding

(defvar xem-keymap nil "Keybinding for `xah-elisp-mode'")
(progn
  (setq xem-keymap (make-sparse-keymap))
  (define-key xem-keymap (kbd "C-c C-<tab>")  'xem-complete-or-indent)
  (define-key xem-keymap (kbd "C-c C-t") 'xem-compact-parens)
  (define-key xem-keymap (kbd "C-c C-u")  'xem-complete-symbol)

  (define-key xem-keymap (kbd "<menu> e <tab>")  'xem-complete-or-indent)
  (define-key xem-keymap (kbd "<menu> e t")  'xem-compact-parens)
  (define-key xem-keymap (kbd "<menu> e u")  'xem-complete-symbol)
  )


;; abbrev and completion

(defun xem--abbrev-position-cursor ()
  ""
  (interactive)
  (when (search-backward "▮" (max 1 (- (point) 300)) t )
    (delete-char 1)
    ))

(put 'xem--abbrev-position-cursor 'no-self-insert t) 

(setq xem-abbrev-table nil)

(define-abbrev-table 'xem-abbrev-table '(

 ;; single letter name has abbrev start with 8
 ("8d" "defun")
 ("8i" "insert")
 ("8l" "let")
 ("8m" "message")
 ("8p" "point")
 ("8s" "setq")
 ("8v" "vector")
 ("8w" "when")

 ("ah" "add-hook")
 ("bc" "backward-char")
 ("bfn" "buffer-file-name")
 ("bmp" "buffer-modified-p")
 ("bol" "beginning-of-line")
 ("botap" "bounds-of-thing-at-point")
 ("bs" "buffer-substring")
 ("bsnp" "buffer-substring-no-properties")
 ("ca" "custom-autoload")
 ("cb" "current-buffer")
 ("cc" "condition-case")
 ("cd" "copy-directory")
 ("cdr" "cdr")
 ("cf" "copy-file")
 ("dc" "delete-char")
 ("dd" "delete-directory")
 ("df" "delete-file")
 ("dk" "define-key")
 ("dr" "delete-region")
 ("efn" "expand-file-name")
 ("eol" "end-of-line")
 ("fc" "forward-char")
 ("ff" "find-file")
 ("fl" "forward-line")
 ("fnd" "file-name-directory")
 ("fne" "file-name-extension")
 ("fnn" "file-name-nondirectory")
 ("fnse" "file-name-sans-extension")
 ("frn" "file-relative-name")
 ("gc" "goto-char")
 ("gnb" "generate-new-buffer")
 ("gsk" "global-set-key")
 ("ifc" "insert-file-contents")
 ("kb" "kill-buffer")
 ("la" "looking-at")
 ("lbp" "line-beginning-position")
 ("lep" "line-end-position")
 ("mb" "match-beginning")
 ("md" "make-directory")
 ("me" "match-end")
 ("mlv" "make-local-variable")
 ("ms" "match-string")
 ("nts" "number-to-string")
 ("ntr" "narrow-to-region")
 ("pm" "point-max")
 ("rap" "region-active-p")
 ("rb" "region-beginning")
 ("re" "region-end")
 ("rf" "rename-file")
 ("rm" "replace-match")
 ("rq" "regexp-quote")
 ("rr" "replace-regexp")
 ("rris" "replace-regexp-in-string")
 ("rsb" "re-search-backward")
 ("rsf" "re-search-forward")
 ("sb" "set-buffer")
 ("sbr" "search-backward-regexp")
 ("sc" "shell-command")
 ("scb" "skip-chars-backward")
 ("scf" "skip-chars-forward")
 ("se" "save-excursion")
 ("sf" "search-forward")
 ("sfm" "set-file-modes")
 ("sfr" "search-forward-regexp")
 ("sm" "string-match")
 ("sr" "save-restriction")
 ("ss" "split-string")
 ("stn" "string-to-number")
 ("str" "string")
 ("tap" "thing-at-point")
 ("wcb" "with-current-buffer")
 ("wg" "widget-get")
 ("yonp" "yes-or-no-p")

("add-hook" "(add-hook HOOK▮ FUNCTION)" xem--abbrev-position-cursor :system t)

("and" "(and ▮)" xem--abbrev-position-cursor :system t )

("append" "(append ▮)" xem--abbrev-position-cursor :system t)

("apply" "(apply ▮)" xem--abbrev-position-cursor :system t)

("aref" "(aref ARRAY▮ INDEX)" xem--abbrev-position-cursor :system t)

("aset" "(aset ARRAY▮ IDX NEWELT)" xem--abbrev-position-cursor :system t)

("assoc" "(assoc KEY▮ LIST)" xem--abbrev-position-cursor :system t)

("assq" "(assq KEY▮ LIST)" xem--abbrev-position-cursor :system t)

("autoload" "(autoload 'FUNCNAME▮ \"FILENAME\" &optional \"DOCSTRING\" INTERACTIVE TYPE)" xem--abbrev-position-cursor :system t)

("backward-char" "(backward-char ▮)" xem--abbrev-position-cursor :system t)

("beginning-of-line" "(beginning-of-line)" xem--abbrev-position-cursor :system t)

("boundp" "(boundp '▮)" xem--abbrev-position-cursor :system t)

("bounds-of-thing-at-point" "(bounds-of-thing-at-point '▮) ; symbol, list, sexp, defun, filename, url, email, word, sentence, whitespace, line, page ...")

("buffer-file-name" "(buffer-file-name)" xem--abbrev-position-cursor :system t)

("buffer-modified-p" "(buffer-modified-p ▮)" xem--abbrev-position-cursor :system t)

("buffer-substring-no-properties" "(buffer-substring-no-properties START▮ END)" xem--abbrev-position-cursor :system t)

("buffer-substring" "(buffer-substring START▮ END)" xem--abbrev-position-cursor :system t)

("called-interactively-p" "(called-interactively-p 'interactive▮)" xem--abbrev-position-cursor :system t)

("car" "(car ▮)" xem--abbrev-position-cursor :system t)

("catch" "(catch TAG▮ BODY)" xem--abbrev-position-cursor :system t)

("cdr" "(cdr ▮)" xem--abbrev-position-cursor :system t)

("concat" "(concat ▮)" xem--abbrev-position-cursor :system t)

("cond" "(cond
(CONDITION▮ BODY)
(CONDITION BODY)
)" xem--abbrev-position-cursor :system t)

("condition-case" "(condition-case ▮)" xem--abbrev-position-cursor :system t)

("cons" "(cons ▮)" xem--abbrev-position-cursor :system t)

("consp" "(consp ▮)" xem--abbrev-position-cursor :system t)

("copy-directory" "(copy-directory ▮ NEWNAME &optional KEEP-TIME PARENTS)" xem--abbrev-position-cursor :system t)

("copy-file" "(copy-file FILE▮ NEWNAME &optional OK-IF-ALREADY-EXISTS KEEP-TIME PRESERVE-UID-GID)" xem--abbrev-position-cursor :system t)

("current-buffer" "(current-buffer)" xem--abbrev-position-cursor :system t)

("custom-autoload" "(custom-autoload ▮ SYMBOL LOAD &optional NOSET)" xem--abbrev-position-cursor :system t)

("defalias" "(defalias 'SYMBOL▮ 'DEFINITION &optional DOCSTRING)" xem--abbrev-position-cursor :system t)

("defconst" "(defconst ▮ INITVALUE \"DOCSTRING\")" xem--abbrev-position-cursor :system t)

("defcustom" "(defcustom ▮ VALUE \"DOC\" &optional ARGS)" xem--abbrev-position-cursor :system t)

("define-key" "(define-key KEYMAPNAME▮ (kbd \"M-b\") 'FUNCNAME)" xem--abbrev-position-cursor :system t)

("defsubst" "(defsubst ▮)" xem--abbrev-position-cursor :system t)

("defun" "(defun ▮ ()
  \"DOCSTRING\"
  (interactive)
  (let (var1)
    
  ))" xem--abbrev-position-cursor :system t)

("defvar" "(defvar ▮ &optional INITVALUE \"DOCSTRING\")" xem--abbrev-position-cursor :system t)

("delete-char" "(delete-char ▮)" xem--abbrev-position-cursor :system t)

("delete-directory" "(delete-directory ▮ &optional RECURSIVE)" xem--abbrev-position-cursor :system t)

("delete-file" "(delete-file ▮)" xem--abbrev-position-cursor :system t)

("delete-region" "(delete-region ▮)" xem--abbrev-position-cursor :system t)

("directory-files" "(directory-files ▮ &optional FULL MATCH NOSORT)" xem--abbrev-position-cursor :system t)

("dolist" "(dolist ▮)" xem--abbrev-position-cursor :system t)

("dotimes" "(dotimes (VAR▮ COUNT [RESULT]) BODY)" xem--abbrev-position-cursor :system t)

("elt" "(elt SEQUENCE▮ N)" xem--abbrev-position-cursor :system t)

("end-of-line" "(end-of-line)" xem--abbrev-position-cursor :system t)

("end-of-line" "(eq ▮)" xem--abbrev-position-cursor :system t)

("equal" "(equal ▮)" xem--abbrev-position-cursor :system t)

("error" "(error \"%s\" ▮)" xem--abbrev-position-cursor :system t)

("expand-file-name" "(expand-file-name ▮ &optional relativedir)" xem--abbrev-position-cursor :system t)

("format" "(format \"▮\" &optional OBJECTS)" xem--abbrev-position-cursor :system t)

("fboundp" "(fboundp '▮)" xem--abbrev-position-cursor :system t)

("file-directory-p" "(file-directory-p ▮)" xem--abbrev-position-cursor :system t)

("file-exists-p" "(file-exists-p ▮)" xem--abbrev-position-cursor :system t)

("file-name-directory" "(file-name-directory ▮)" xem--abbrev-position-cursor :system t)

("file-name-extension" "(file-name-extension ▮ &optional PERIOD)" xem--abbrev-position-cursor :system t)

("file-name-nondirectory" "(file-name-nondirectory ▮)" xem--abbrev-position-cursor :system t)

("file-name-sans-extension" "(file-name-sans-extension ▮)" xem--abbrev-position-cursor :system t)

("file-regular-p" "(file-regular-p ▮)" xem--abbrev-position-cursor :system t)

("file-relative-name" "(file-relative-name ▮)" xem--abbrev-position-cursor :system t)

("find-file" "(find-file ▮)" xem--abbrev-position-cursor :system t)

("format" "(format \"%s\" ▮)" xem--abbrev-position-cursor :system t)

("forward-char" "(forward-char ▮)" xem--abbrev-position-cursor :system t)

("forward-line" "(forward-line ▮)" xem--abbrev-position-cursor :system t)

("funcall" "(funcall ▮)" xem--abbrev-position-cursor :system t)

("function" "(function ▮)" xem--abbrev-position-cursor :system t)

("generate-new-buffer" "(generate-new-buffer ▮)" xem--abbrev-position-cursor :system t)

("get" "(get SYMBOL▮ PROPNAME)" xem--abbrev-position-cursor :system t)

("global-set-key" "(global-set-key (kbd \"C-▮\") 'COMMAND)" xem--abbrev-position-cursor :system t)

("goto-char" "(goto-char ▮)" xem--abbrev-position-cursor :system t)

("if" "(if ▮
    (progn )
  (progn )
)" xem--abbrev-position-cursor :system t)

("insert-file-contents" "(insert-file-contents ▮ &optional VISIT BEG END REPLACE)" xem--abbrev-position-cursor :system t)

("insert" "(insert ▮)" xem--abbrev-position-cursor :system t)

("interactive" "(interactive)" xem--abbrev-position-cursor :system t)

("kbd" "(kbd \"▮\")" xem--abbrev-position-cursor :system t)

("kill-buffer" "(kill-buffer ▮)" xem--abbrev-position-cursor :system t)

("lambda" "(lambda (▮) BODY)" xem--abbrev-position-cursor :system t)

("length" "(length ▮)" xem--abbrev-position-cursor :system t)

("let" "(let (▮)
 x
)" xem--abbrev-position-cursor :system t)

("line-beginning-position" "(line-beginning-position)" xem--abbrev-position-cursor :system t)

("line-end-position" "(line-end-position)" xem--abbrev-position-cursor :system t)

("list" "(list ▮)" xem--abbrev-position-cursor :system t)

("looking-at" "(looking-at ▮)" xem--abbrev-position-cursor :system t)

("make-directory" "(make-directory ▮ &optional PARENTS)" xem--abbrev-position-cursor :system t)

("make-local-variable" "(make-local-variable ▮)" xem--abbrev-position-cursor :system t)

("mapc" "(mapc '▮ SEQUENCE)" xem--abbrev-position-cursor :system t)

("mapcar" "(mapcar '▮ SEQUENCE)" xem--abbrev-position-cursor :system t)

("mapconcat" "(mapconcat FUNCTION▮ SEQUENCE SEPARATOR)" xem--abbrev-position-cursor :system t)

("match-beginning" "(match-beginning N▮)" xem--abbrev-position-cursor :system t)

("match-end" "(match-end N▮)" xem--abbrev-position-cursor :system t)

("match-string" "(match-string ▮)" xem--abbrev-position-cursor :system t)

("max" "(max ▮)" xem--abbrev-position-cursor :system t)

("member" "(member ▮ LIST)" xem--abbrev-position-cursor :system t)

("memq" "(memq ▮ LIST)" xem--abbrev-position-cursor :system t)

("message" "(message \"%s▮\" ARGS)" xem--abbrev-position-cursor :system t)

("min" "(min ▮)" xem--abbrev-position-cursor :system t)

("narrow-to-region" "(narrow-to-region START▮ END)" xem--abbrev-position-cursor :system t)

("not" "(not ▮)" xem--abbrev-position-cursor :system t)

("nth" "(nth N▮ LIST)" xem--abbrev-position-cursor :system t)

("null" "(null ▮)" xem--abbrev-position-cursor :system t)

("number-to-string" "(number-to-string ▮)" xem--abbrev-position-cursor :system t)

("or" "(or ▮)" xem--abbrev-position-cursor :system t)

("point-max" "(point-max)" xem--abbrev-position-cursor :system t)

("point-min" "(point-min)" xem--abbrev-position-cursor :system t)

("point" "(point)" xem--abbrev-position-cursor :system t)

("prin1" "(prin1 ▮)" xem--abbrev-position-cursor :system t)

("princ" "(princ ▮)" xem--abbrev-position-cursor :system t)

("print" "(print ▮)" xem--abbrev-position-cursor :system t)

("progn" "(progn ▮)" xem--abbrev-position-cursor :system t)

("push" "(push ▮)" xem--abbrev-position-cursor :system t)

("put" "(put 'SYMBOL▮ PROPNAME VALUE)" xem--abbrev-position-cursor :system t)

("random" "(random ▮)" xem--abbrev-position-cursor :system t)

("rassoc" "(rassoc KEY▮ LIST)" xem--abbrev-position-cursor :system t)

("re-search-backward" "(re-search-backward REGEXP▮ &optional BOUND NOERROR COUNT)" xem--abbrev-position-cursor :system t)

("re-search-forward" "(re-search-forward REGEXP▮ &optional BOUND NOERROR COUNT)" xem--abbrev-position-cursor :system t)

("read-directory-name" "(read-directory-name \"▮\" &optional DIR DEFAULT-DIRNAME MUSTMATCH INITIAL)" xem--abbrev-position-cursor :system t)

("read-file-name" "(read-file-name \"▮\" &optional DIR DEFAULT-FILENAME MUSTMATCH INITIAL PREDICATE)" xem--abbrev-position-cursor :system t)

("read-regexp" "(read-regexp \"▮\" &optional DEFAULT-VALUE)" xem--abbrev-position-cursor :system t)

("read-string" "(read-string \"▮\" &optional INITIAL-INPUT HISTORY DEFAULT-VALUE INHERIT-INPUT-METHOD)" xem--abbrev-position-cursor :system t)

("regexp-opt" "(regexp-opt STRINGS▮ &optional PAREN)" xem--abbrev-position-cursor :system t)

("regexp-quote" "(regexp-quote ▮)" xem--abbrev-position-cursor :system t)

("region-active-p" "(region-active-p)" xem--abbrev-position-cursor :system t)

("region-beginning" "(region-beginning)" xem--abbrev-position-cursor :system t)

("region-end" "(region-end)" xem--abbrev-position-cursor :system t)

("rename-file" "(rename-file FILE▮ NEWNAME &optional OK-IF-ALREADY-EXISTS)" xem--abbrev-position-cursor :system t)

("repeat" "(repeat ▮)" xem--abbrev-position-cursor :system t)

("replace-match" "(replace-match NEWTEXT▮ &optional FIXEDCASE LITERAL STRING SUBEXP)" xem--abbrev-position-cursor :system t)

("replace-regexp-in-string" "(replace-regexp-in-string REGEXP▮ REP STRING &optional FIXEDCASE LITERAL SUBEXP START)" xem--abbrev-position-cursor :system t)

("replace-regexp" "(replace-regexp REGEXP▮ TO-STRING &optional DELIMITED START END)" xem--abbrev-position-cursor :system t)

("require" "(require ▮)" xem--abbrev-position-cursor :system t)

("reverse" "(reverse ▮)" xem--abbrev-position-cursor :system t)

("save-buffer" "(save-buffer ▮)" xem--abbrev-position-cursor :system t)

("save-excursion" "(save-excursion ▮)" xem--abbrev-position-cursor :system t)

("save-restriction" "(save-restriction ▮)" xem--abbrev-position-cursor :system t)

("search-backward-regexp" "(search-backward-regexp \"▮\" &optional BOUND NOERROR COUNT)" xem--abbrev-position-cursor :system t)

("search-backward" "(search-backward \"▮\" &optional BOUND NOERROR COUNT)" xem--abbrev-position-cursor :system t)

("search-forward-regexp" "(search-forward-regexp \"▮\" &optional BOUND NOERROR COUNT)" xem--abbrev-position-cursor :system t)

("search-forward" "(search-forward \"▮\" &optional BOUND NOERROR COUNT)" xem--abbrev-position-cursor :system t)

("set-buffer" "(set-buffer ▮)" xem--abbrev-position-cursor :system t)

("set-file-modes" "(set-file-modes ▮ MODE)" xem--abbrev-position-cursor :system t)

("set-mark" "(set-mark ▮)" xem--abbrev-position-cursor :system t)

("set" "(set ▮)" xem--abbrev-position-cursor :system t)

("setq" "(setq ▮)" xem--abbrev-position-cursor :system t)

("shell-command" "(shell-command ▮ &optional OUTPUT-BUFFER ERROR-BUFFER)" xem--abbrev-position-cursor :system t)

("skip-chars-backward" "(skip-chars-backward \"▮\" &optional LIM)" xem--abbrev-position-cursor :system t)

("skip-chars-forward" "(skip-chars-forward \"▮\" &optional LIM)" xem--abbrev-position-cursor :system t)

("split-string" "(split-string ▮ &optional SEPARATORS OMIT-NULLS)" xem--abbrev-position-cursor :system t)

("string-match-p" "(string-match-p \"REGEXP▮\" \"STRING\" &optional START)" xem--abbrev-position-cursor :system t)

("string-match" "(string-match \"REGEXP▮\" \"STRING\" &optional START)" xem--abbrev-position-cursor :system t)

("string-to-number" "(string-to-number \"▮\")" xem--abbrev-position-cursor :system t)

("string" "(string ▮)" xem--abbrev-position-cursor :system t)

("string=" "(string= ▮)" xem--abbrev-position-cursor :system t)

("stringp" "(stringp ▮)" xem--abbrev-position-cursor :system t)

("substring-no-properties" "(substring-no-properties ▮ FROM TO)" xem--abbrev-position-cursor :system t)

("substring" "(substring STRING▮ FROM &optional TO)" xem--abbrev-position-cursor :system t)

("thing-at-point" "(thing-at-point '▮) ; symbol, list, sexp, defun, filename, url, email, word, sentence, whitespace, line, page ...")

("throw" "(throw TAG▮ VALUE)" xem--abbrev-position-cursor :system t)

("unless" "(unless ▮)" xem--abbrev-position-cursor :system t)

("vector" "(vector ▮)" xem--abbrev-position-cursor :system t)

("when" "(when ▮)" xem--abbrev-position-cursor :system t)

("while" "(while ▮)" xem--abbrev-position-cursor :system t)

("widget-get" "(widget-get ▮)" xem--abbrev-position-cursor :system t)

("with-current-buffer" "(with-current-buffer ▮)" xem--abbrev-position-cursor :system t)

("with-temp-buffer" "(with-temp-buffer ▮)" xem--abbrev-position-cursor :system t)

("with-temp-file" "(with-temp-file FILE▮)" xem--abbrev-position-cursor :system t)

("write-file" "(write-file FILENAME▮ &optional CONFIRM)" xem--abbrev-position-cursor :system t)

("write-region" "(write-region (point-min) (point-max) FILENAME &optional APPEND VISIT LOCKNAME MUSTBENEW)" xem--abbrev-position-cursor :system t)

;; #name: process marked files in dired
;; # --
;; ;; idiom for processing a list of files in dired's marked files

;; ;; suppose myProcessFile is your function that takes a file path
;; ;; and do some processing on the file

;; (defun dired-myProcessFile ()
;;   "apply myProcessFile function to marked files in dired."
;;   (interactive)
;;   (require 'dired)
;;   (mapc 'myProcessFile (dired-get-marked-files))
;; )

;; ;; to use it, type M-x dired-myProcessFile

;; #name: a function that process a file
;; # --
;; (defun doThisFile (fpath)
;;   "Process the file at path FPATH ..."
;;   (let ()
;;     ;; create temp buffer without undo record or font lock. (more efficient)
;;     ;; first space in temp buff name is necessary
;;     (set-buffer (get-buffer-create " myTemp"))
;;     (insert-file-contents fpath nil nil nil t)

;;     ;; process it ...
;;     ;; (goto-char 0) ; move to begining of file's content (in case it was open)
;;     ;; ... do something here
;;     ;; (write-file fpath) ;; write back to the file

;;     (kill-buffer " myTemp")))

;;

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

("y-or-n-p" "(y-or-n-p \"PROMPT▮ \")" xem--abbrev-position-cursor :system t)

("yes-or-no-p" "(yes-or-no-p \"PROMPT▮ \")" xem--abbrev-position-cursor :system t)

 )

"abbrev table for `xah-elisp-mode'"
:regexp "\\_<\\([_-A-Za-z]+\\)"
  )

;; define the mode
(defun xah-elisp-mode ()
    "A major mode for emacs lisp.

Emacs lisp keywords are colored.
and other experimental features.

eventual plan is:
• there shall be no command to indent code, except one that reformat code semantically, not by line. preferably transparent to user as she types. Code formatting shall never be programer's concern.
• no reliance on emacs's syntax table
• no reliance on emacs's comment-dwim
• no reliance on yasnippet or any third-party package.
• everything shall be elisp only. not rely on shell tool or lang engines. (which can be later added)

\\{xem-keymap}"
    (interactive)

  (kill-all-local-variables)

  (setq mode-name "∑lisp")
  (setq major-mode 'xah-elisp-mode)
  (setq font-lock-defaults '((xem-font-lock-keywords)))

  (set-syntax-table emacs-lisp-mode-syntax-table)
  (use-local-map xem-keymap)
  (setq local-abbrev-table xem-abbrev-table)
  (setq abbrev-mode t)

  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1) ;default to `;;' in comment-region
  (setq-local comment-column 2)

  (setq-local indent-line-function 'lisp-indent-line)
  ;; (setq-local indent-region-function 'xem-indent-region)
  (setq-local tab-always-indent 'complete)

  (yas-minor-mode 0) ; todo. temp

  (add-hook 'completion-at-point-functions 'xem-complete-symbol nil 'local)

  (progn
  ;; setup auto-complete-mode
    (when (fboundp 'auto-complete-mode)
      (add-to-list 'ac-modes 'xah-elisp-mode)
      ;; (add-hook 'xah-elisp-mode-hook 'ac-emacs-lisp-mode-setup)
      )
    )

  (run-mode-hooks 'xah-elisp-mode-hook)
  )

(provide 'xah-elisp-mode)
