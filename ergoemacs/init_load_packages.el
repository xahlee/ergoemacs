; -*- coding: utf-8 -*-

;;;; language modes

;; php mode
(autoload 'php-mode "php-mode" "php mode by Aaron S Hawley." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; javascript mode
(autoload 'js2-mode "js2-20080616a" "Steve Yegge's Javascript mode." t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; visual-basic-mode
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(add-to-list 'auto-mode-alist '("\\.vbs\\'" . visual-basic-mode)) ;VBscript
(add-to-list 'auto-mode-alist '("\\.vb\\'" . visual-basic-mode))  ;visual basic .NET file
(add-to-list 'auto-mode-alist '("\\.bas\\'" . visual-basic-mode)) ;visual basic form
(add-to-list 'auto-mode-alist '("\\.frm\\'" . visual-basic-mode)) ;basic language source
(add-to-list 'auto-mode-alist '("\\.cls\\'" . visual-basic-mode)) ;C++ class definition file

;; powershell-mode
(autoload 'powershell-mode "powershell-mode" "A editing mode for Microsoft PowerShell." t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode)) ; PowerShell script

;; powershell interactive shell
(autoload 'powershell "powershell" "Interactive shell for PowerShell." t)

;; xlsl-mode
(autoload 'xlsl-mode "xlsl-mode" "Load xlsl-mode for editing Linden Scripting Lang." t)
(add-to-list 'auto-mode-alist '("\\.lsl\\'" . xlsl-mode))

;; AutoHotKey mode (a keyboard macro for Windows)
(setq ahk-syntax-directory "c:/Program Files (x86)/AutoHotkey/Extras/Editors/Syntax/")
(autoload 'ahk-mode "ahk-mode" "AutoHotKey mode" t)
(add-to-list 'auto-mode-alist '("\\.ahk\\'" . ahk-mode))

;; POV-Ray (3D rendering engine)
(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/pov-mode-3.2/"))
(autoload 'pov-mode "pov-mode" "Major mode for working with POV-Ray code." t)
(add-to-list 'auto-mode-alist '("\\.pov\\'" . pov-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . pov-mode))

;; mode for Haskell lang
(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/haskell-mode-2.4/"))
(load "haskell-mode-2.4/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; tuareg mode for ML/Caml/OCaml lang
(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/tuareg-mode-1.45.6/")
             )
(add-to-list 'auto-mode-alist '("\\.ml\\w?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing ML/Caml/OCaml code." t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;;;; productivity, enhancement, or minor modes

; redo mode
(require 'redo)

;; speck-mode
(autoload 'speck-mode "speck" "On-the-fly spell checking mode, alternative to fly-spell." t)

;; Hunspell
(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/rw-hunspell/")
	     )
(require 'rw-hunspell nil t)

;; yasnippet template system
(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/yasnippet-0.6.1c/")
             )
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory
 (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/yasnippet-0.6.1c/snippets/")
 )

;; command-frequency minor mode
(autoload 'command-frequency "command-frequency" "minor mode for logging emacs command calls for usage insights by statistics." t)

;; display horizontal line for page break char ^L
(require 'pp-c-l)
(setq pp^L-^L-string "                                                           ")
(pretty-control-l-mode 1)

;; dictionary client for dict.org 
(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name buffer-file-name)) "../packages/dictionary-1.8.7")
)
(autoload 'dictionary-search "dictionary" "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary" "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary-lookup-definition "dictionary" "Unconditionally lookup the word at point." t)
(autoload 'dictionary "dictionary" "Create a new dictionary buffer" t)
(autoload 'dictionary-mouse-popup-matching-words "dictionary" "Display entries matching the word at the cursor" t)
(autoload 'dictionary-popup-matching-words "dictionary" "Display entries matching the word at the point" t)
(autoload 'dictionary-tooltip-mode "dictionary" "Display tooltips for the current word" t)
(autoload 'global-dictionary-tooltip-mode "dictionary" "Enable/disable dictionary-tooltip-mode for all buffers" t)