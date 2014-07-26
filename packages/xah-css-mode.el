;;; xah-css-mode.el --- Major mode for editing CSS code. -*- coding: utf-8 -*-

;; Copyright © 2013 by Xah Lee

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;; Created: 2013-04-18
;; Keywords: languages, convenience

;; You can redistribute this program and/or modify it. Please give credit and link. Thanks.

;;; Commentary:
;; Major mode for editing CSS code. Beta stage.
;; home page http://ergoemacs.org/emacs/xah-css-mode.html

;;; HISTORY

;; version history no longer kept here.
;; version 0.3, 2013-05-02 added xcm-hex-color-to-hsl, and other improvements.
;; version 0.2, 2013-04-22 added xcm-compact-css-region
;; version 0.1, 2013-04-18 first version

(require 'xfrp_find_replace_pairs)
;(require 'xeu_elisp_util)
(require 'color) ; part of emacs 24.3

(defvar xah-css-mode-hook nil "Standard hook for `xah-css-mode'")



(defun xcm-insert-random-color-hsl ()
  "Insert a random color string of CSS HSL format.
Example output: hsl(100,24%,82%);"
  (interactive)
  (insert (format "hsl(%d,%d%%,%d%%);" (random 360) (random 100) (random 100))) )

(defun xcm-hex-color-to-hsl ()
  "Convert color spec under cursor from “#rrggbb” to CSS HSL format.
 ⁖ #ffefd5 → hsl(37,100%,91%)
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
          (insert (xcm-hex-to-hsl-color currentWord )))
      (progn
        (error "The current word 「%s」 is not of the form #rrggbb." currentWord)
        )
      )))

(defun xcm-hex-to-hsl-color (hexStr)
  "Convert hexStr color to CSS HSL format.
Return a string.
 ⁖
 (xcm-hex-to-hsl-color \"#ffefd5\") ⇒ \"hsl(37,100%,91%)\"
"
  (let* (
         (colorVec (xcm-convert-color-hex-to-vec hexStr))
         (xR (elt colorVec 0))
         (xG (elt colorVec 1))
         (xB (elt colorVec 2))
         (hsl (color-rgb-to-hsl xR xG xB) )
         (xH (elt hsl 0))
         (xS (elt hsl 1))
         (xL (elt hsl 2))
         )
    (format "hsl(%d,%d%%,%d%%)" (* xH 360) (* xS 100) (* xL 100) )
    ))

(defun xcm-convert-color-hex-to-vec (hexcolor)
  "Convert HEXCOLOR from “\"rrggbb\"” string to a elisp vector [r g b], where the values are from 0 to 1.
Example:
 (xcm-convert-color-hex-to-vec \"00ffcc\") ⇒ [0.0 1.0 0.8]

Note: The input string must NOT start with “#”. If so, the return value is nil."
  (vector
   (xcm-normalize-number-scale (string-to-number (substring hexcolor 0 2) 16) 255)
   (xcm-normalize-number-scale (string-to-number (substring hexcolor 2 4) 16) 255)
   (xcm-normalize-number-scale (string-to-number (substring hexcolor 4) 16) 255)
   ))

(defun xcm-normalize-number-scale (myVal rangeMax)
  "Return a number between [0, 1] that's a rescaled myVal.
myVal's original range is [0, rangeMax].

The arguments can be int or float.
Return value is float.
"
  (/ (float myVal) (float rangeMax)))


;;; functions

(defun xcm-compact-css-region (p1 p2)
  "Remove unnecessary whitespaces of CSS source code in region.
WARNING: not robust."
  (interactive "r")
  (save-restriction
    (narrow-to-region p1 p2)
    (replace-regexp-pairs-region (point-min) (point-max) '(["  +" " "]))
    (replace-pairs-region (point-min) (point-max)
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
                            )) ) )


(defvar xcm-html-tag-names nil "a list of HTML5 tag names.")
(setq xcm-html-tag-names
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

(defvar xcm-property-names nil "a list of CSS property names.")
(setq xcm-property-names
'(
"background"
"background-color"
"background-image"
"background-position"
"background-size"
"background-repeat"
"border"
"border-bottom"
"border-collapse"
"border-color"
"border-left"
"border-radius"
"border-right"
"border-style"
"border-top"
"border-width"
"box-shadow"
"clear"
"color"
"content"
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
"max-width"
"min-width"
"opacity"
"orphans"
"overflow"
"padding"
"padding-left"
"padding-right"
"padding-top"
"padding-bottom"
"page-break-after"
"page-break-inside"
"position"
"pre-wrap"
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

"border-top-left-radius"
"border-top-right-radius"
"border-bottom-right-radius"
"border-bottom-left-radius"

) )

(defvar xcm-pseudo-selector-names nil "a list of CSS pseudo selector names.")
(setq xcm-pseudo-selector-names '(
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

(defvar xcm-media-keywords nil "a list of CSS xxxxx todo.")
(setq xcm-media-keywords '(
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

(defvar xcm-unit-names nil "a list of CSS unite names.")
(setq xcm-unit-names '("px"
"pt"
"pc"
"cm"
"mm"
"in"
"em"
"ex"
"%") )

(defvar xcm-value-kwds nil "a list of CSS value names")
(setq xcm-value-kwds
'(
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
"rtl"
"sans-serif"
"serif"
"small"
"smaller"
"solid"
"square"
"static"
"thin"
"top"
"transparent"
"underline"
"url"
"x-large"
"xx-large"
) )

(defvar xcm-color-names nil "a list of CSS color names.")
(setq xcm-color-names
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

(defvar xcm-all-keywords nil "list of all elisp keywords")
(setq xcm-all-keywords (append xcm-html-tag-names
                                     xcm-color-names
                                     xcm-property-names
                                     xcm-pseudo-selector-names
                                     xcm-media-keywords
                                     xcm-unit-names
                                     xcm-value-kwds
                                     ))


;; completion

(defun xcm-complete-symbol ()
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
          (ido-completing-read "" xcm-all-keywords nil nil ξcurrent-sym ))
    (delete-region ξp1 ξp2)
    (insert ξresult-sym)

    ))


;; syntax table
(defvar xcm-syntax-table nil "Syntax table for `xah-css-mode'.")
(setq xcm-syntax-table
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

(setq xcm-font-lock-keywords
      (let (
          (htmlTagNames (regexp-opt xcm-html-tag-names 'words) )
          (cssPropertieNames (regexp-opt xcm-property-names 'symbols ) )
          (cssValueNames (regexp-opt xcm-value-kwds 'symbols ) )
          (cssColorNames (regexp-opt xcm-color-names 'symbols) )
          (cssUnitNames (regexp-opt xcm-unit-names 'symbols ) )
          (cssPseudoSelectorNames (regexp-opt xcm-pseudo-selector-names ) )
          (cssMedia (regexp-opt xcm-media-keywords ) )
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

(defun xcm-complete-or-indent ()
  "Do keyword completion or indent/prettify-format.

If char before point is letters and char after point is whitespace or punctuation, then do completion, except when in string or comment. In these cases, do `xcm-prettify-root-sexp'."
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
          (xcm-prettify-root-sexp))
      (progn (if
                 (and (looking-back "[-_a-zA-Z]")
                      (or (eobp) (looking-at "[\n[:blank:][:punct:]]")))
                 (xcm-complete-symbol)
               (xcm-indent-line))))))

(defun xcm-indent-line ()
  "i do nothing."
  (let ()
    nil))



(defun xcm-abbrev-enable-function ()
  "Determine whether to expand abbrev.
This is called by emacs abbrev system."
  (let ((ξsyntax-state (syntax-ppss)))
    (if (or (nth 3 ξsyntax-state) (nth 4 ξsyntax-state))
        (progn nil)
      t)))

(setq xcm-abbrev-table nil)

(define-abbrev-table 'xcm-abbrev-table
  '(

    ("bgc" "background-color" nil :system t))

  "abbrev table for `xah-css-mode'"
  ;; :regexp "\\_<\\([_-0-9A-Za-z]+\\)"
  :regexp "\\([_-0-9A-Za-z]+\\)"
  :case-fixed t
  :enable-function 'xcm-abbrev-enable-function
  )


;; keybinding

(when (string-equal system-type "windows-nt")
  (define-key key-translation-map (kbd "<apps>") (kbd "<menu>")))

(defvar xcm-keymap nil "Keybinding for `xah-css-mode'")

(progn
  (setq xcm-keymap (make-sparse-keymap))
  (define-key xcm-keymap (kbd "<tab>") 'xcm-complete-or-indent)

  (define-key xcm-keymap (kbd "<menu> e r") 'xcm-insert-random-color-hsl)
  (define-key xcm-keymap (kbd "<menu> e c") 'xcm-hex-color-to-hsl)
  (define-key xcm-keymap (kbd "<menu> e p") 'xcm-compact-css-region)
  (define-key xcm-keymap (kbd "<menu> e u") 'xcm-complete-symbol)
  (define-key xcm-keymap (kbd "<menu> e i") 'xcm-indent-line)

  ;  (define-key xcm-keymap [remap comment-dwim] 'xcm-comment-dwim)
  )



(require 'prog-mode)

;; define the mode
(define-derived-mode xah-css-mode prog-mode
  "ξCSS "
  "A major mode for CSS.

CSS keywords are colored. Basically that's it.

\\{xcm-keymap}"

  (set-syntax-table xcm-syntax-table)
  (setq font-lock-defaults '((xcm-font-lock-keywords)))

  (setq local-abbrev-table xcm-abbrev-table)
  (set (make-local-variable 'comment-start) "/*")
  (set (make-local-variable 'comment-start-skip) "/\\*+[ \t]*")
  (set (make-local-variable 'comment-end) "*/")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\*+/")
  (use-local-map xcm-keymap)

  (run-mode-hooks 'xah-css-mode-hook)
)

(when (featurep 'auto-complete )
  (add-to-list 'ac-modes 'xah-css-mode)
  (add-hook 'xah-css-mode-hook 'ac-css-mode-setup)
  )

(provide 'xah-css-mode)

;; 2013-05-01

;; complete list of css property names, as of 2013-05-01, from https://developer.mozilla.org/en-US/docs/CSS/CSS_Reference

;; many of them are not that common in use. Some are experimental from CSS3.

;; align-content
;; align-items
;; align-self
;; animation
;; animation-delay
;; animation-direction
;; animation-duration
;; animation-fill-mode
;; animation-iteration-count
;; animation-name
;; animation-play-state
;; animation-timing-function
;; attr
;; auto
;; backface-visibility
;; background
;; background-attachment
;; background-clip
;; background-color
;; background-image
;; background-origin
;; background-position
;; background-repeat
;; background-size
;; border
;; border-bottom
;; border-bottom-color
;; border-bottom-left-radius
;; border-bottom-right-radius
;; border-bottom-style
;; border-bottom-width
;; border-collapse
;; border-color
;; border-image
;; border-image-outset
;; border-image-repeat
;; border-image-slice
;; border-image-source
;; border-image-width
;; border-left
;; border-left-color
;; border-left-style
;; border-left-width
;; border-radius
;; border-right
;; border-right-color
;; border-right-style
;; border-right-width
;; border-spacing
;; border-style
;; border-top
;; border-top-color
;; border-top-left-radius
;; border-top-right-radius
;; border-top-style
;; border-top-width
;; border-width
;; bottom
;; box-decoration-break
;; box-shadow
;; box-sizing
;; break-after
;; break-before
;; break-inside
;; calc()
;; caption-side
;; clear
;; clip
;; clip-path
;; color
;; column-count
;; column-fill
;; column-gap
;; column-rule
;; column-rule-color
;; column-rule-style
;; column-rule-width
;; column-span
;; column-width
;; columns
;; content
;; counter-increment
;; counter-reset
;; cross-fade()
;; cubic-bezier()
;; cursor
;; cycle()
;; direction
;; display
;; element()
;; empty-cells
;; filter
;; flex
;; flex-basis
;; flex-direction
;; flex-flow
;; flex-grow
;; flex-shrink
;; flex-wrap
;; float
;; font
;; font-family
;; font-feature-settings
;; font-kerning
;; font-language-override
;; font-size
;; font-size-adjust
;; font-stretch
;; font-style
;; font-variant
;; font-variant-ligatures
;; font-weight
;; height
;; hsl()
;; hsla()
;; hyphens
;; icon
;; image()
;; image-orientation
;; image-rendering
;; image-resolution
;; ime-mode
;; inherit
;; initial
;; justify-content
;; left
;; letter-spacing
;; line-height
;; linear-gradient()
;; list-style
;; list-style-image
;; list-style-position
;; list-style-type
;; margin
;; margin-bottom
;; margin-left
;; margin-right
;; margin-top
;; marks
;; mask
;; matrix()
;; matrix3d()
;; max-height
;; max-width
;; min-height
;; min-width
;; nav-down
;; nav-index
;; nav-left
;; nav-right
;; nav-up
;; none
;; normal
;; object-fit
;; object-position
;; opacity
;; order
;; orphans
;; outline
;; outline-color
;; outline-offset
;; outline-style
;; outline-width
;; overflow
;; overflow-wrap
;; overflow-x
;; overflow-y
;; padding
;; padding-bottom
;; padding-left
;; padding-right
;; padding-top
;; page-break-after
;; page-break-before
;; page-break-inside
;; perspective
;; perspective()
;; perspective-origin
;; pointer-events
;; position
;; quotes
;; radial-gradient()
;; rect()
;; repeating-linear-gradient()
;; repeating-radial-gradient()
;; resize
;; rgb()
;; rgba()
;; right
;; rotate()
;; rotate3d()
;; rotateX()
;; rotateY()
;; rotateZ()
;; scale()
;; scale3d()
;; scaleX()
;; scaleY()
;; scaleZ()
;; skew()
;; skewX()
;; skewY()
;; steps()
;; tab-size
;; table-layout
;; text-align
;; text-align-last
;; text-combine-horizontal
;; text-decoration
;; text-decoration-color
;; text-decoration-line
;; text-decoration-style
;; text-indent
;; text-orientation
;; text-overflow
;; text-rendering
;; text-shadow
;; text-transform
;; text-underline-position
;; top
;; transform
;; transform-origin
;; transform-style
;; transition
;; transition-delay
;; transition-duration
;; transition-property
;; transition-timing-function
;; translate()
;; translate3d()
;; translateX()
;; translateY()
;; translateZ()
;; unicode-bidi
;; url()
;; var()
;; var-*
;; vertical-align
;; visibility
;; white-space
;; widows
;; width
;; word-break
;; word-spacing
;; word-wrap
;; writing-mode
;; z-index
