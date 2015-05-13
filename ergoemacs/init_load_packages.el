; -*- coding: utf-8 -*-

(require 'dired-x)

;; makes it possible to have some modes not have linum-mode on when global-linum-mode is on
;; 2013-04-19, as of GNU Emacs 24.3.1, linum-mode will freeze emacs for about 10 minutes when opening a jpg file of 10 megabytes size
(require 'linum-off)

;; ; redo mode
(add-to-list 'load-path (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/") )
(require 'undo-tree)
(defalias 'redo 'undo-tree-redo)
(global-undo-tree-mode 1)


;;;; language modes

;;; php mode
(autoload 'php-mode "php-mode" "php mode by Aaron S Hawley." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;;; visual-basic-mode
(autoload 'visual-basic-mode "visual-basic-mode" "Major moder for editing Visual Basic code." t)
(add-to-list 'auto-mode-alist '("\\.vbs\\'" . visual-basic-mode)) ;VBscript
(add-to-list 'auto-mode-alist '("\\.vb\\'" . visual-basic-mode))  ;visual basic .NET file
(add-to-list 'auto-mode-alist '("\\.bas\\'" . visual-basic-mode)) ;visual basic form
(add-to-list 'auto-mode-alist '("\\.frm\\'" . visual-basic-mode)) ;basic language source
(add-to-list 'auto-mode-alist '("\\.cls\\'" . visual-basic-mode)) ;C++ class definition file

;;; csharp mode
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;;; for editing Windows's cmd.exe's script; batch, “.bat” file mode.
(autoload 'dos-mode "dos" "A mode for editing Windows cmd.exe batch scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat\\'" . dos-mode))
(add-to-list 'auto-mode-alist '("\\.cmd\\'" . dos-mode))

;;; powershell-mode. http://en.wikipedia.org/wiki/PowerShell
(autoload 'powershell-mode "powershell-mode" "A editing mode for Microsoft PowerShell scripts." t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode)) ; PowerShell script

;;; powershell interactive shell
(when (string-equal system-type "windows-nt")
  (autoload 'powershell "powershell" "Interactive shell for Microsoft PowerShell." t)
)


;;;; productivity, enhancement, or minor modes

;;; enhanced execute-extended-command
(require 'smex)
(smex-initialize)

;; enhanced text selection, expand-region
;; 2013-04-19 turned it off. Because, it's slow, and often incorrect. e.g. in js-mode, itone case took it few seconds …
;(add-to-list 'load-path (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/expand-region/"))
;(require 'expand-region)
;(when
;    (member 'expand-region features)
;  (if
;      (fboundp 'ergoemacs-key )
;      (progn (ergoemacs-key "M-8" 'er/expand-region "←region→")
;             (ergoemacs-key "M-9" 'er/contract-region "→region←")
;             )
;    (progn
;      (global-set-key (kbd "M-8") 'er/expand-region)
;      (global-set-key (kbd "M-9") 'er/contract-region)
;      )
;    )
;  )

;; display horizontal line for the Form Feed char (ASCII 12, ^L)
;; The Form Feed char is often used in elisp source code for marking sections. The command forward-page (and backward-page) moves to the next form feed char.
;; (require 'pp-c-l)

;;; make the formfeed char (^L) display as a line
(require 'page-break-lines)
(global-page-break-lines-mode 1)

;; record command call statistics
(require 'keyfreq)
(setq keyfreq-file "~/.emacs.d/.emacs.keyfreq")
(setq keyfreq-file-lock "~/.emacs.d/.emacs.keyfreq.lock")
(setq keyfreq-autosave-timeout 600)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; ;; loads tramp. This is to fix a dired recursive load bug, see: http://ergoemacs.org/emacs/emacs_on_ubuntu_linux.html
;; (require 'tramp)
