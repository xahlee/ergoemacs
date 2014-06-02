;-*- coding: utf-8 -*-

;; changing font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-normal-size)

(global-set-key (kbd "C-S-z") 'redo)

(defun add-browser-backspace-key-to-Info-mode ()
  "Add some browser styled navigation keys for `Info-mode'.
  The following keys are added:
 【Backspace】 for `Info-history-back'
 【Shift+Backspace】 for `Info-history-forward'."
  (progn
    (local-set-key (kbd "<backspace>") 'Info-history-back)
    (local-set-key (kbd "<S-backspace>") 'Info-history-forward)
    )
  ;; note: on Linux Firefox, you have to turn on Backspace key for previous page. In the preference.
  )

(add-hook 'Info-mode-hook 'add-browser-backspace-key-to-Info-mode)

;; In Windows, Alt+F4 closes the frame (or kill emacs if it is the last frame)
(if (and (boundp 'w32-initialized) w32-initialized)
    (global-set-key (kbd "M-<f4>") 'close-frame))
