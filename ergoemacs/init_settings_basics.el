; -*- coding: utf-8 -*-
;; config for things that are bundled with emacs 24

;; UTF-8 as default encoding
(set-language-environment "UTF-8")


;;; dired, file, related

;; for ergoemacs-new-empty-buffer
(setq initial-major-mode (quote text-mode))

;; make dired list not inclued 「.」 and 「..」, and use metric prefix for file size
(setq dired-listing-switches "-Al --si --time-style long-iso")

(setq mark-ring-max 5)

;; don't create backup~ or #auto-save# files
(setq backup-by-copying t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; make dired suggest target dir (for copy, move, …) that's in the other dired pane
(setq dired-dwim-target t)

;; make dired allow deleting/copy whole dir
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

;; Save minibuffer history
(savehist-mode 1)

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

;; turn on save place so that when opening a file, the cursor will be at the last position.
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "saveplace.el") ) ; use standard emacs dir
(setq-default save-place t)

(setq enable-recursive-minibuffers t )

;; used standard emacs dir
(setq bookmark-default-file (concat user-emacs-directory "bookmarks.el") )

;; apache per dir config file
(add-to-list 'auto-mode-alist '("\\.htaccess\\'" . conf-unix-mode))


;;; editing related

;; make cursor movement stop in between camelCase words.
(when (fboundp 'global-subword-mode ) (global-subword-mode 1))

;; make typing delete/overwrites selected text
(delete-selection-mode 1)

;; set highlighting brackets
(show-paren-mode 1)
(setq show-paren-style 'expression)

(when (>= emacs-major-version 24)
;; typing opening brackets auto insert closing one
(electric-pair-mode 1)
;; setting for auto-close brackets for electric-pair-mode regardless of current major mode syntax table
(setq electric-pair-pairs '( (?\" . ?\") (?\{ . ?\}) ) )
 )

;; make 【Ctrl+c】 for copy, 【Ctrl+x】 for cut, etc.
;; (cua-mode 1) ; now part of ergoemacs-mode

;; Alt+y is not cua-repeat-replace-region
;; (define-key cua--cua-keys-keymap [(meta v)] 'nil)

(progn
  ;; interactive name completion for describe-function, describe-variable, execute-extended-command, etc.
  (icomplete-mode 1)

  ;; make icomplete prettier
  (setq icomplete-separator "\n")
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-in-buffer t)

)

(progn
  ;; make buffer switch command do suggestions, also for find-file command
  (ido-mode 1)
  (setq ido-separator "\n")
  (setq ido-enable-flex-matching t) ; show any name that has the chars you typed
)

;; display line numbers at margin
(global-linum-mode 1)

(setq tab-always-indent 'complete)

;; majority of code formatting conventions do no recommend mixed tabs and spaces. So, here.
(setq-default indent-tabs-mode nil)     ; emacs 23.1 default is t

;; seems 4 is more popular than 8. Need more research.
(setq tab-width 4)   ; width for display tabs. emacs 23.1 default is 8

(progn
  ;; org-mode

  ;; Make lines not dissapear into the right margin while in “org-mode”
  (add-hook 'org-mode-hook 'soft-wrap-lines)

  ;; make “org-mode” syntax color code sections
  (setq org-src-fontify-natively t)
  )

;; when calling “list-colors-display”, make result sorted by hue.
(when (>= emacs-major-version 24) (setq list-colors-sort 'hsv ) )

(progn
  ;; seems pointless to warn. There's always undo.
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)
)

;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )


;; make buffer names unique when files of the same name in different dirs are opened
(require 'uniquify) ; bundled with GNU emacs 23.2.1 or before. On in 24.4
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; emacs 24.4 style

(progn
  ;; Make whitespace-mode with very basic background coloring for whitespaces.
  ;; http://ergoemacs.org/emacs/whitespace-mode.html
  (setq whitespace-style (quote ( spaces tabs newline space-mark tab-mark newline-mark )))

  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [182 10]) ; LINE FEED,
          (tab-mark 9 [9655 9] [92 9]) ; tab
          )) )

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 ;; '(diredp-ignored-file-name ((t (:foreground "#bebebe"))))
 ;; '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))))
 ;; '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))))
 ;; '(isearch ((((class color) (min-colors 88) (background light)) (:background "black" :foreground "white"))))
 '(completions-common-part ((t (:inherit default :foreground "red"))))
 '(show-paren-match ((((class color) (background light)) (:background "azure2"))))
 )