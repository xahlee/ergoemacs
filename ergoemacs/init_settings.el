; -*- coding: utf-8 -*-


;;; Make emacs open all files in last emacs session.

;; This functionality is provided by desktop-save-mode (“feature” name: “desktop”). The mode is not on by default in emacs 23.1, and has a lot options. The following is init settings for the mode for ErgoEmacs.

;; Goal: have emacs always auto open the set of opened files in last session, even if emacs crashed in last session or the OS crashed in last session. Also, don't bother users by asking questions like “do you want to save desktop?” or “do you want to override last session file?”, because these are annoying and terms like “session” or “desktop” are confusing to most users because it can have many meanings.

;; Some tech detail: set the desktop session file 〔.emacs.desktop〕 at the variable “user-emacs-directory” (default value is “~/.emacs.d/”).  This file is our desktop file. It will be auto created and or over-written.  if a emacs expert has other desktop session files elsewhere, he can still use or manage those.

(require 'desktop)

(defun desktop-file-modtime-reset ()
  "Reset `desktop-file-modtime' so the user is not bothered."
  (interactive)
  (run-with-timer 5 nil
          (lambda ()
            (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
            (desktop-save user-emacs-directory))))

(defun desktop-settings-setup ()
  "Some settings setup for desktop-save-mode."
  (interactive)

  ;; At this point the desktop.el hook in after-init-hook was executed, so (desktop-read) is avoided.
  (when (not (eq (emacs-pid) (desktop-owner))) ; Check that emacs did not load a desktop yet
    ;; Here we activate the desktop mode
    (desktop-save-mode 1)

    ;; The default desktop is saved always
    (setq desktop-save t)

    ;; The default desktop is loaded anyway if it is locked
    (setq desktop-load-locked-desktop t)

    ;; Set the location to save/load default desktop
    (setq desktop-dirname user-emacs-directory)

    ;; Make sure that even if emacs or OS crashed, emacs still have last opened files.
    (add-hook 'find-file-hook 'desktop-file-modtime-reset)

    ;; Read default desktop
    (if (file-exists-p (concat desktop-dirname desktop-base-file-name))
        (desktop-read desktop-dirname))

    ;; Add a hook when emacs is closed to we reset the desktop modification time (in this way the user does not get a warning message about desktop modifications)
    (add-hook 'kill-emacs-hook 'desktop-file-modtime-reset)
    )
  )

(defun hide-init-buffers ()
  "hide some buffers when emacs starts.
No splash screen. and If the *scratch* buffer is the current one, then create a new empty untitled buffer to hide *scratch*
"
  (interactive)
  (progn
    (setq inhibit-startup-screen t)
    (if (string= (buffer-name) "*scratch*")
        (let ((buf (generate-new-buffer "untitled")))
          (switch-to-buffer buf)
          (funcall (and initial-major-mode))
          (setq buffer-offer-save t))
        ) ))

(add-hook 'after-init-hook 'desktop-settings-setup "APPEND")
(add-hook 'after-init-hook 'hide-init-buffers "APPEND")

(setq page-break-lines-modes (quote (emacs-lisp-mode xah-elisp-mode compilation-mode fundamental-mode text-mode org-mode ruby-mode python-mode xah-html-mode html-mode nxml-mode )) )


;; compile elisp files after save, do so only if there exists a byte-compiled file
;; thanks to Adolfo Benedetti, 2011-07-15
(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

;; auto compile elisp files after save.
;; (add-hook 'emacs-lisp-mode-hook (lambda () (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t)) )


;; For htmlize.el
;; Rationale: use unicode whenever possible, since it's widely supported today.
(setq htmlize-convert-nonascii-to-entities nil) ; make htmlize generate unicode directly instead of html entities
(setq htmlize-html-charset "utf-8") ; make the output html use utf-8 charset



;; (progn
;; ;; use cperl-mode instead of perl-mode
;; ;; 2013-05-13 turn off for now. seems too hairy, coloring is better but doesn't color some simple variable 「$xxxx」.
;;   (setq auto-mode-alist (rassq-delete-all 'perl-mode auto-mode-alist))
;;   (add-to-list 'auto-mode-alist '("\\.\\(p\\([lm]\\)\\)\\'" . cperl-mode))
;;   (setq interpreter-mode-alist (rassq-delete-all 'perl-mode interpreter-mode-alist))
;;   (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
;;   (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
;;   (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
;;   )
