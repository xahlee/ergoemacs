;;; xah-css-mode.el --- Major mode for editing CSS code. -*- coding: utf-8 -*-

;; Copyright © 2013 by Xah Lee

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;; Created: 2013-04-18
;; Keywords: languages, convenience

;; You can redistribute this program and/or modify it. Please give credit and link. Thanks.

;;; Commentary:
;; Major mode for editing CSS code.
;; home page http://ergoemacs.org/emacs/xah-css-mode.html

;;; HISTORY

;; version history no longer kept here.
;; version 2015-01-30 fix a problem with emacs 24.3.1, Debugger entered--Lisp error: (file-error "Cannot open load file" "prog-mode")
;; version 0.3, 2013-05-02 added xah-css-hex-color-to-hsl, and other improvements.
;; version 0.2, 2013-04-22 added xah-css-compact-css-region
;; version 0.1, 2013-04-18 first version

(require 'xah-replace-pairs)
;(require 'xeu_elisp_util)
(require 'color) ; part of emacs 24.3

(defvar xah-css-mode-hook nil "Standard hook for `xah-css-mode'")



(defun xah-css-insert-random-color-hsl ()
  "Insert a random color string of CSS HSL format.
Example output: hsl(100,24%,82%);"
  (interactive)
  (insert (format "hsl(%d,%d%%,%d%%);" (random 360) (random 100) (random 100))) )

(defun xah-css-hex-color-to-hsl ()
  "Convert color spec under cursor from “#rrggbb” to CSS HSL format.
 ⁖ #ffefd5 ⇒ hsl(37,100%,91%)
"
  (interactive)
  (let* (
         (bds (bounds-of-thing-at-point 'word))
         (p1 (car bds))
         (p2 (cdr bds))
         (currentWord (buffer-substring-no-properties p1 p2)))

    (if (string-match "[a-fA-F0-9]\\{6\\}" currentWord)
        (progn
          (delete-region p1 p2 )
          (if (looking-back "#") (delete-char -1))
          (insert (xah-css-hex-to-hsl-color currentWord )))
      (progn
        (error "The current word 「%s」 is not of the form #rrggbb." currentWord)))))

(defun xah-css-hex-to-hsl-color (φhex-str)
  "Convert φhex-str color to CSS HSL format.
Return a string.
 ⁖
  \"#ffefd5\" ⇒ \"hsl(37,100%,91%)\"
"
  (let* (
         (colorVec (xah-css-convert-color-hex-to-vec φhex-str))
         (xR (elt colorVec 0))
         (xG (elt colorVec 1))
         (xB (elt colorVec 2))
         (hsl (color-rgb-to-hsl xR xG xB))
         (xH (elt hsl 0))
         (xS (elt hsl 1))
         (xL (elt hsl 2)))
    (format "hsl(%d,%d%%,%d%%)" (* xH 360) (* xS 100) (* xL 100))))

(defun xah-css-convert-color-hex-to-vec (φhexcolor)
  "Convert φhexcolor from “\"rrggbb\"” string to a elisp vector [r g b], where the values are from 0 to 1.
Example:
 (xah-css-convert-color-hex-to-vec \"00ffcc\") ⇒ [0.0 1.0 0.8]

Note: The input string must NOT start with “#”. If so, the return value is nil."
  (vector
   (xah-css-normalize-number-scale (string-to-number (substring φhexcolor 0 2) 16) 255)
   (xah-css-normalize-number-scale (string-to-number (substring φhexcolor 2 4) 16) 255)
   (xah-css-normalize-number-scale (string-to-number (substring φhexcolor 4) 16) 255)
   ))

(defun xah-css-normalize-number-scale (φval φrange-max)
  "scale φval from range [0, φrange-max] to [0, 1]
The arguments can be int or float.
Return value is float."
  (/ (float φval) (float φrange-max)))


;;; functions

(defun xah-css-compact-css-region (φbegin φend)
  "Remove unnecessary whitespaces of CSS source code in region.
WARNING: not robust.
URL `http://ergoemacs.org/emacs/elisp_css_compressor.html'
Version 2015-04-29"
  (interactive "r")
  (save-restriction
    (narrow-to-region φbegin φend)
    (xah-replace-regexp-pairs-region
     (point-min)
     (point-max)
     '(["  +" " "]))
    (xah-replace-pairs-region
     (point-min)
     (point-max)
     '(
       ["\n" ""]
       [" /* " "/*"]
       [" */ " "*/"]
       [" {" "{"]
       ["{ " "{"]
       ["; " ";"]
       [": " ":"]
       [";}" "}"]
       ["}" "}\n"]
       ))))


(defvar xah-css-html-tag-names nil "a list of HTML5 tag names.")
(setq xah-css-html-tag-names
'("a"
"abbr"
"address"
"applet"
"area"
"article"
"aside"
"audio"
"b"
"base"
"basefont"
"bdi"
"bdo"
"blockquote"
"body"
"br"
"button"
"canvas"
"caption"
"cite"
"code"
"col"
"colgroup"
"command"
"datalist"
"dd"
"del"
"details"
"dfn"
"div"
"dl"
"doctype"
"dt"
"em"
"embed"
"fieldset"
"figcaption"
"figure"
"footer"
"form"
"h1"
"h2"
"h3"
"h4"
"h5"
"h6"
"head"
"header"
"hgroup"
"hr"
"html"
"i"
"iframe"
"img"
"input"
"ins"
"kbd"
"keygen"
"label"
"legend"
"li"
"link"
"map"
"mark"
"menu"
"meta"
"meter"
"nav"
"noscript"
"object"
"ol"
"optgroup"
"option"
"output"
"p"
"param"
"pre"
"progress"
"q"
"rp"
"rt"
"ruby"
"s"
"samp"
"script"
"section"
"select"
"small"
"source"
"span"
"strong"
"style"
"sub"
"summary"
"sup"
"table"
"tbody"
"td"
"textarea"
"tfoot"
"th"
"thead"
"time"
"title"
"tr"
"u"
"ul"
"var"
"video"
"wbr")
 )

(defvar xah-css-property-names nil "a list of CSS property names.")
(setq xah-css-property-names
'(

"align-content"
"align-items"
"align-self"
"animation"
"animation-delay"
"animation-direction"
"animation-duration"
"animation-fill-mode"
"animation-iteration-count"
"animation-name"
"animation-play-state"
"animation-timing-function"
"attr"
"backface-visibility"
"background"
"background-attachment"
"background-clip"
"background-color"
"background-image"
"background-origin"
"background-position"
"background-repeat"
"background-size"
"border"
"border-bottom"
"border-bottom-color"
"border-bottom-left-radius"
"border-bottom-right-radius"
"border-bottom-style"
"border-bottom-width"
"border-collapse"
"border-color"
"border-image"
"border-image-outset"
"border-image-repeat"
"border-image-slice"
"border-image-source"
"border-image-width"
"border-left"
"border-left-color"
"border-left-style"
"border-left-width"
"border-radius"
"border-right"
"border-right-color"
"border-right-style"
"border-right-width"
"border-spacing"
"border-style"
"border-top"
"border-top-color"
"border-top-left-radius"
"border-top-right-radius"
"border-top-style"
"border-top-width"
"border-width"
"bottom"
"box-decoration-break"
"box-shadow"
"box-sizing"
"break-after"
"break-before"
"break-inside"
"clear"
"color"
"content"
"counter-increment"
"counter-reset"
"cursor"
"direction"
"display"
"filter"
"float"
"font"
"font-family"
"font-size"
"font-style"
"font-weight"
"height"
"letter-spacing"
"line-height"
"list-style"
"list-style-image"
"list-style-type"
"margin"
"margin-bottom"
"margin-left"
"margin-right"
"margin-top"
"max-height"
"max-width"
"min-height"
"min-width"
"opacity"
"orphans"
"overflow"
"padding"
"padding-bottom"
"padding-left"
"padding-right"
"padding-top"
"page-break-after"
"page-break-inside"
"position"
"pre-wrap"

"tab-size"
"table-layout"
"text-align"
"text-align-last"
"text-combine-horizontal"
"text-decoration"
"text-decoration-color"
"text-decoration-line"
"text-decoration-style"
"text-indent"
"text-orientation"
"text-overflow"
"text-rendering"
"text-shadow"
"text-transform"
"text-underline-position"
"top"
"transform"
"transform-origin"
"transform-style"
"transition"
"transition-delay"
"transition-duration"
"transition-property"
"transition-timing-function"

"table"
"table-cell"
"text-align"
"text-decoration"
"text-shadow"

"unicode-bidi"
"vertical-align"
"white-space"
"widows"
"width"
"word-spacing"
"word-wrap"
"z-index"

) )

(defvar xah-css-pseudo-selector-names nil "a list of CSS pseudo selector names.")
(setq xah-css-pseudo-selector-names '(
"::after"
"::before"
"::choices"
"::first-letter"
"::first-line"
"::repeat-index"
"::repeat-item"
"::selection"
"::value"
":active"
":after"
":before"
":checked"
":default"
":dir"
":disabled"
":empty"
":enabled"
":first"
":first-child"
":first-letter"
":first-line"
":first-of-type"
":focus"
":fullscreen"
":hover"
":in-range"
":indeterminate"
":invalid"
":lang"
":last-child"
":last-of-type"
":left"
":link"
":not"
":nth-child"
":nth-last-child"
":nth-last-of-type"
":nth-of-type"
":only-child"
":only-of-type"
":optional"
":out-of-range"
":read-only"
":read-write"
":required"
":right"
":root"
":scope"
":target"
":valid"
":visited"

) )

(defvar xah-css-media-keywords nil "a list of CSS xxxxx todo.")
(setq xah-css-media-keywords '(
"@charset"
"@document"
"@font-face"
"@import"
"@keyframes"
"@media"
"@namespace"
"@page"
"@supports"
"@viewport"
"print"
"screen"
"all"
"speech"
) ) ; todo

(defvar xah-css-unit-names nil "a list of CSS unite names.")
(setq xah-css-unit-names '("px"
"pt"
"pc"
"cm"
"mm"
"in"
"em"
"rem"
"ex"
"%") )

(defvar xah-css-value-kwds nil "a list of CSS value names")
(setq xah-css-value-kwds
'(

"flex"
"grid"

"flex-wrap"
"wrap"
"flex-start"

"!important"
"absolute"
"alpha"
"auto"
"avoid"
"block"
"bold"
"both"
"bottom"
"break-word"
"center"
"collapse"
"dashed"
"dotted"
"embed"
"fixed"
"help"
"hidden"
"hsl"
"hsla"
"inherit"
"inline"
"inline-block"
"italic"
"large"
"left"
"line-through"
"ltr"
"middle"
"monospace"
"no-repeat"
"none"
"normal"
"nowrap"
"pointer"
"relative"
"rgb"
"rgba"
"right"
"rotate"
"rotate3d"
"rotateX"
"rotateY"
"rotateZ"
"rtl"
"sans-serif"
"scale"
"scale3d"
"scaleX"
"scaleY"
"scaleZ"
"serif"
"skew"
"skewX"
"skewY"
"small"
"smaller"
"solid"
"square"
"static"
"steps"
"thin"
"top"
"transparent"
"translate"
"translate3d"
"translateX"
"translateY"
"translateZ"
"underline"
"url"
"x-large"
"xx-large"

) )

(defvar xah-css-color-names nil "a list of CSS color names.")
(setq xah-css-color-names
'("aliceblue"
"antiquewhite"
"aqua"
"aquamarine"
"azure"
"beige"
"bisque"
"black"
"blanchedalmond"
"blue"
"blueviolet"
"brown"
"burlywood"
"cadetblue"
"chartreuse"
"chocolate"
"coral"
"cornflowerblue"
"cornsilk"
"crimson"
"cyan"
"darkblue"
"darkcyan"
"darkgoldenrod"
"darkgray"
"darkgreen"
"darkgrey"
"darkkhaki"
"darkmagenta"
"darkolivegreen"
"darkorange"
"darkorchid"
"darkred"
"darksalmon"
"darkseagreen"
"darkslateblue"
"darkslategray"
"darkslategrey"
"darkturquoise"
"darkviolet"
"deeppink"
"deepskyblue"
"dimgray"
"dimgrey"
"dodgerblue"
"firebrick"
"floralwhite"
"forestgreen"
"fuchsia"
"gainsboro"
"ghostwhite"
"gold"
"goldenrod"
"gray"
"green"
"greenyellow"
"grey"
"honeydew"
"hotpink"
"indianred"
"indigo"
"ivory"
"khaki"
"lavender"
"lavenderblush"
"lawngreen"
"lemonchiffon"
"lightblue"
"lightcoral"
"lightcyan"
"lightgoldenrodyellow"
"lightgray"
"lightgreen"
"lightgrey"
"lightpink"
"lightsalmon"
"lightseagreen"
"lightskyblue"
"lightslategray"
"lightslategrey"
"lightsteelblue"
"lightyellow"
"lime"
"limegreen"
"linen"
"magenta"
"maroon"
"mediumaquamarine"
"mediumblue"
"mediumorchid"
"mediumpurple"
"mediumseagreen"
"mediumslateblue"
"mediumspringgreen"
"mediumturquoise"
"mediumvioletred"
"midnightblue"
"mintcream"
"mistyrose"
"moccasin"
"navajowhite"
"navy"
"oldlace"
"olive"
"olivedrab"
"orange"
"orangered"
"orchid"
"palegoldenrod"
"palegreen"
"paleturquoise"
"palevioletred"
"papayawhip"
"peachpuff"
"peru"
"pink"
"plum"
"powderblue"
"purple"
"red"
"rosybrown"
"royalblue"
"saddlebrown"
"salmon"
"sandybrown"
"seagreen"
"seashell"
"sienna"
"silver"
"skyblue"
"slateblue"
"slategray"
"slategrey"
"snow"
"springgreen"
"steelblue"
"tan"
"teal"
"thistle"
"tomato"
"turquoise"
"violet"
"wheat"
"white"
"whitesmoke"
"yellow"
"yellowgreen")
 )

(defvar xah-css-all-keywords nil "list of all elisp keywords")
(setq xah-css-all-keywords (append xah-css-html-tag-names
                                     xah-css-color-names
                                     xah-css-property-names
                                     xah-css-pseudo-selector-names
                                     xah-css-media-keywords
                                     xah-css-unit-names
                                     xah-css-value-kwds
                                     ))


;; completion

(defun xah-css-complete-symbol ()
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
          (ido-completing-read "" xah-css-all-keywords nil nil ξcurrent-sym ))
    (delete-region ξp1 ξp2)
    (insert ξresult-sym)

    ))


;; syntax table
(defvar xah-css-syntax-table nil "Syntax table for `xah-css-mode'.")
(setq xah-css-syntax-table
      (let ((synTable (make-syntax-table)))

;        (modify-syntax-entry ?0  "." synTable)
;        (modify-syntax-entry ?1  "." synTable)
;        (modify-syntax-entry ?2  "." synTable)
;        (modify-syntax-entry ?3  "." synTable)
;        (modify-syntax-entry ?4  "." synTable)
;        (modify-syntax-entry ?5  "." synTable)
;        (modify-syntax-entry ?6  "." synTable)
;        (modify-syntax-entry ?7  "." synTable)
;        (modify-syntax-entry ?8  "." synTable)
;        (modify-syntax-entry ?9  "." synTable)

        (modify-syntax-entry ?_ "_" synTable)
        (modify-syntax-entry ?: "." synTable)

        (modify-syntax-entry ?- "_" synTable)
        (modify-syntax-entry ?\/ ". 14" synTable) ; /* java style comment*/
        (modify-syntax-entry ?* ". 23" synTable)
        synTable))


;; syntax coloring related

(setq xah-css-font-lock-keywords
      (let (
          (htmlTagNames (regexp-opt xah-css-html-tag-names 'words) )
          (cssPropertieNames (regexp-opt xah-css-property-names 'symbols ) )
          (cssValueNames (regexp-opt xah-css-value-kwds 'symbols) )
          (cssColorNames (regexp-opt xah-css-color-names 'symbols) )
          (cssUnitNames (regexp-opt xah-css-unit-names 'symbols ) )
          (cssPseudoSelectorNames (regexp-opt xah-css-pseudo-selector-names ) )
          (cssMedia (regexp-opt xah-css-media-keywords ) )
          )
        `(
          (,cssPropertieNames . font-lock-type-face)
          (,cssValueNames . font-lock-keyword-face)
          (,cssColorNames . font-lock-constant-face)
          (,cssUnitNames . font-lock-builtin-face)
          (,cssPseudoSelectorNames . font-lock-preprocessor-face)
          (,cssMedia . font-lock-reference-face)
          (,htmlTagNames . font-lock-function-name-face)

("#[abcdef[:digit:]]\\{6\\}" .
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))

("hsl( *\\([0-9]\\{1,3\\}\\) *, *\\([0-9]\\{1,3\\}\\)% *, *\\([0-9]\\{1,3\\}\\)% *)" .
     (0 (put-text-property
         (+ (match-beginning 0) 3)
         (match-end 0)
         'face (list :background
 (concat "#" (mapconcat 'identity
                        (mapcar
                         (lambda (x) (format "%02x" (round (* x 255))))
                         (color-hsl-to-rgb
                          (/ (string-to-number (match-string-no-properties 1)) 360.0)
                          (/ (string-to-number (match-string-no-properties 2)) 100.0)
                          (/ (string-to-number (match-string-no-properties 3)) 100.0)
                          ) )
                        "" )) ;  "#00aa00"
                      ))))

          ("'[^']+'" . font-lock-string-face)
          ) ) )


;; indent/reformat related

(defun xah-css-complete-or-indent ()
  "Do keyword completion or indent/prettify-format.

If char before point is letters and char after point is whitespace or punctuation, then do completion, except when in string or comment. In these cases, do `xah-css-prettify-root-sexp'."
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
          (xah-css-prettify-root-sexp))
      (progn (if
                 (and (looking-back "[-_a-zA-Z]")
                      (or (eobp) (looking-at "[\n[:blank:][:punct:]]")))
                 (xah-css-complete-symbol)
               (xah-css-indent-line))))))

(defun xah-css-indent-line ()
  "i do nothing."
  (let ()
    nil))



(defun xah-css-abbrev-enable-function ()
  "Determine whether to expand abbrev.
This is called by emacs abbrev system."
  (let ((ξsyntax-state (syntax-ppss)))
    (if (or (nth 3 ξsyntax-state) (nth 4 ξsyntax-state))
        (progn nil)
      t)))

(setq xah-css-abbrev-table nil)

(define-abbrev-table 'xah-css-abbrev-table
  '(

    ("bgc" "background-color" nil :system t)
    ("rgb" "rgb(▮)" nil :system t)
    ("rgba" "rgba(▮)" nil :system t)
    ("rotate" "rotate(▮9deg)" nil :system t)
    ("rotate3d" "rotate3d(▮)" nil :system t)
    ("rotateX" "rotateX(▮)" nil :system t)
    ("rotateY" "rotateY(▮)" nil :system t)
    ("rotateZ" "rotateZ(▮)" nil :system t)
    ("scale" "scale(▮)" nil :system t)
    ("scale3d" "scale3d(▮)" nil :system t)
    ("scaleX" "scaleX(▮)" nil :system t)
    ("scaleY" "scaleY(▮)" nil :system t)
    ("scaleZ" "scaleZ(▮)" nil :system t)
    ("skew" "skew(▮9deg)" nil :system t)
    ("skewX" "skewX(▮)" nil :system t)
    ("skewY" "skewY(▮)" nil :system t)
    ("steps" "steps(▮)" nil :system t)

    ("translate" "translate(▮px,▮px)" nil :system t)
    ("translate3d" "translate3d(▮)" nil :system t)
    ("translateX" "translateX(▮)" nil :system t)
    ("translateY" "translateY(▮)" nil :system t)
    ("translateZ" "translateZ(▮)" nil :system t)

)

  "abbrev table for `xah-css-mode'"
  ;; :regexp "\\_<\\([_-0-9A-Za-z]+\\)"
  :regexp "\\([_-0-9A-Za-z]+\\)"
  :case-fixed t
  ;; :enable-function 'xah-css-abbrev-enable-function
  )


;; keybinding

(when (string-equal system-type "windows-nt")
  (define-key key-translation-map (kbd "<apps>") (kbd "<menu>")))

(defvar xah-css-keymap nil "Keybinding for `xah-css-mode'")

(progn
  (setq xah-css-keymap (make-sparse-keymap))
  (define-key xah-css-keymap (kbd "TAB") 'xah-css-complete-or-indent)

  (define-prefix-command 'xah-css-single-keys-keymap)

  (define-key xah-css-single-keys-keymap (kbd "r") 'xah-css-insert-random-color-hsl)
  (define-key xah-css-single-keys-keymap (kbd "c") 'xah-css-hex-color-to-hsl)
  (define-key xah-css-single-keys-keymap (kbd "p") 'xah-css-compact-css-region)
  (define-key xah-css-single-keys-keymap (kbd "u") 'xah-css-complete-symbol)
  (define-key xah-css-single-keys-keymap (kbd "i") 'xah-css-indent-line)

  ;  (define-key xah-css-keymap [remap comment-dwim] 'xah-css-comment-dwim)
  )



;; define the mode
(defun xah-css-mode ()
  "A major mode for CSS.

CSS keywords are colored. Basically that's it.

\\{xah-css-keymap}"
  (interactive)
  (kill-all-local-variables)

  (setq mode-name "∑CSS")
  (setq major-mode 'xah-css-mode)

  (set-syntax-table xah-css-syntax-table)
  (setq font-lock-defaults '((xah-css-font-lock-keywords)))

  (if (or
       (not (boundp 'xfk-major-mode-lead-key))
       (null 'xfk-major-mode-lead-key))
      (define-key xah-css-keymap (kbd "<menu> e") xah-css-single-keys-keymap)
    (define-key xah-css-keymap xfk-major-mode-lead-key xah-css-single-keys-keymap))
  (use-local-map xah-css-keymap)

  (setq local-abbrev-table xah-css-abbrev-table)
  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")

  (run-mode-hooks 'xah-css-mode-hook))

;; (when (featurep 'auto-complete )
;;   (add-to-list 'ac-modes 'xah-css-mode)
;;   (add-hook 'xah-css-mode-hook 'ac-css-mode-setup))

(provide 'xah-css-mode)

