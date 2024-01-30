[![MELPA](https://melpa.org/packages/accent-badge.svg)](https://melpa.org/#/accent)

# accent.el

## Overview

accent.el shows a popup with accented characters based on the current letter under the cursor.

Based on the MacOS features for adding accented letters with a long keypress.

## Usage

Position the cursor on the character to transform and press `C-x C-a` (require binding configuration).

- Select the accented character from the popup with `Enter`
- Close the popup with `C-g`

If the character has no accents available, a message will be prompted accordingly.

### Screenshots

![screen](etc/img/screen.png)

Here I'm using `cursor-type: 'block` and `accent-position: 'after`.

## Installation

You can install `accent.el` from [MELPA](https://melpa.org/#/accent).

## Config

### key binding - RECOMMENDED

Bind a key sequence to open the `accent-menu` accordingly.

``` emacs-lisp
(global-set-key (kbd "C-x C-a") 'accent-menu)
```

`C-x C-a` is a good choice if not already bound. To find your emacs key bindings use

``` emacs-lisp
C-h k key-sequence
```

### accent-position

``` emacs-lisp
;; Use the character after the cursor instead of before
(setq accent-position 'after)
```

### accent-custom

Append a list of custom accents to the default collection.

``` emacs-lisp
(setq accent-custom '((a (ă))
                      (o (ŏ))
                      (u (ŭ))))
```

### `company-mode`

If you have [company-mode](https://github.com/company-mode/company-mode) installed, you can use `accent-company` instead of `accent-menu`. It probably doesn't make much sense to add it to `company-backends`, but using it standalone shall suffice if you like `company` more than `popup.el`.

---

## Articles

- https://xenodium.com/an-accentuated-emacs-experiment/

## License
Released under GPLv3.
