;;; xah-insert-random-id.el --- commands to insert random ID. -*- coding: utf-8 -*-

;; Copyright © 2013 by Xah Lee

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;; Maintainer: Xah Lee
;; Created: 2013-04-19
;; Version: 0.1
;; Keywords: convenience

;; feel free to use this code in anyway you like. Credit and donation is appreciated. Thanks.

;;; Commentary:
;; misc collection of commands to insert hex, digits, of fixed number of digits.

;;; Install:
;; (require 'xah-insert-random-id)

;;; Todo:
;; code needs to be refactored, so that there are a bunch of functions that returns a value, then one interactive command wrapper to them. Also, the command need to accept universal-argument for number of digits to insert.

;;; Change Log:
;; • 0.2 2014-04-23 package name changed from insert-random-id.el to xah-insert-random-id.el. And some other minor updates.
;; • 0.1 2013-04-19 first version. Moved from my personal init.

(random t) ; set seed

(defun xah-insert-random-hex (φn)
  "Insert φn random hexidecimal digits.
φn default to 8"
  (interactive "P")
  (let* ((ξcharset "0123456789abcdef" )
         (ξbaseCount (length ξcharset)))
    (dotimes (ξi (if (numberp φn) (abs φn) 8 ))
      (insert (elt ξcharset (random ξbaseCount))))))

(defun xah-insert-random-string (φn)
  "Insert a random alphanumerics string of length 10.
The possible chars are 0 to 9, and a to z (lower case)."
  (interactive "P")
  (let* ((ξcharset "1234567890abcdefghijklmnopqrstuvwxyz")
         (ξbaseCount (length ξcharset)))
    (dotimes (ξi (if (numberp φn) (abs φn) 10))
      (insert (elt ξcharset (random ξbaseCount))))))

(defun xah-insert-random-uuid ()
  "Insert a UUID. This uses a simple hashing of variable data.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d

Note: this code uses https://en.wikipedia.org/wiki/Md5, which is not cryptographically safe. I'm not sure what's the implication of its use here.

Version 2015-01-30
URL `http://ergoemacs.org/emacs/elisp_generate_uuid.html'
"
;; by Christopher Wellons, 2011-11-18. Editted by Xah Lee.
;; Edited by Hideki Saito further to generate all valid variants for "N" in xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx format.
  (interactive)
  (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                            (user-uid)
                            (emacs-pid)
                            (system-name)
                            (user-full-name)
                            (current-time)
                            (emacs-uptime)
                            (garbage-collect)
                            (buffer-string)
                            (random)
                            (recent-keys)))))

    (insert (format "%s-%s-4%s-%s%s-%s"
                    (substring myStr 0 8)
                    (substring myStr 8 12)
                    (substring myStr 13 16)
                    (format "%x" (+ 8 (random 4)))
                    (substring myStr 17 20)
                    (substring myStr 20 32)))))

(defun xah-insert-random-number (φn)
  "Insert φn of random digits.
φn default to 5"
  (interactive "P")
  (let (ξcharset (ξbaseCount 10))
    (setq ξcharset "1234567890" )
    (dotimes (ξi (if (numberp φn) (abs φn) 5 ))
      (insert (elt ξcharset (random ξbaseCount))) ) ) )

(provide 'xah-insert-random-id)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xah-insert-random-id.el ends here
