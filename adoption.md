# Tips For Adopting ErgoEmacs Keybindings #

If you are a long time emacs user, you may find it painful to adopt this setup.

This difficulty is nothing special. It's the same difficulty when you switching to dvorak after years of qwerty. Basically, it's about changing muscle memory.

## Where Did My Command Go ##

The ergonomic-mode minor mode features the command where-is-old-binding, with shortcut “Ctrl+h o”. This command asks you to type a shortcut, and tells you which command it was bound in GNU Emacs, and the new shortcut for it under ErgoEmacs.

## Shortcut To Open Cheatsheet ##

You can define a key such as F8 to open the image file showing the layout.
Put the following in your emacs init file:

```
;; open keyboard shortcut image with F8 key
(global-set-key (kbd "<f8>")
  (lambda ()
    (interactive)
    (find-file "~/.emacs.d/ergonomic_emacs_layout_qwerty.png")))
```

You need to download the png image at
> http://xahlee.org/emacs/ergonomic_emacs_keybinding.html
image for qwerty, dvorak are all available there.

place the image in your emacs load dir at “~/.emacs.d/”.

## Gradual Adoption ##

Here's some tips that may help you adopt.

If you find it too painful to switch, don't use the whole package. Instead, start off with just the arrow key movements.

```
;; turn off ergoemacs keybinding
(ergoemacs-mode 0)

;; Single char cursor movement. (assuming you are on qwerty)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-SPC") 'set-mark-command)

;; kicking the habit
(global-unset-key (kbd "C-b")) ; backward-char
(global-unset-key (kbd "C-f")) ; forward-char
(global-unset-key (kbd "C-p")) ; previous-line
(global-unset-key (kbd "C-n")) ; next-line
(global-unset-key (kbd "C-SPC")) ; set-mark-command
```

Put the above in your emacs init file (usually at “~/.emacs”).

With only the above change, you will increase your emacs productivity, especially if you are a touch typist. These single char cursor moving commands are the top 4 most frequently used emacs commands by statistics, roughly accounting for 43% of commands that have a shortcut.

You don't necessary have to unset the default bindings like the above, but it is recommended, because otherwise you'll just fall back to habit.

Once you used the above for a couple of weeks, you may add more keys to adopt.

The code above does not remap keys in special modes, such as in shell, in mini-buffer, or some other modes. When in these modes, just use the arrow key. The full ergoemacs mode has code that take care of these modes once you are ready to switch fully.

### Level 2 ###

Adding keys for moving around words and deleting words.

```
;; kicking the habit
(global-unset-key (kbd "M-f")) ; forward-word
(global-unset-key (kbd "M-b")) ; backward-word
(global-unset-key (kbd "M-d")) ; kill-word
(global-unset-key (kbd "C-<backspace>")) ; backward-kill-word
(global-unset-key (kbd "C-d")) ; delete-char

;; move by word
(global-set-key (kbd "M-u") 'backward-word)
(global-set-key (kbd "M-o") 'forward-word) ; was (prefix)

;; Delete previous/next char.
(global-set-key (kbd "M-d") 'delete-backward-char)
(global-set-key (kbd "M-f") 'delete-char)

; Delete previous/next word.
(global-set-key (kbd "M-e") 'backward-kill-word)
(global-set-key (kbd "M-r") 'kill-word)
```

### Level 3 ###

Try to use the full ergoemacs mode.