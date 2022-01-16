# accent.el

## Overview

accent.el shows a popup with accented characters based on the current letter under the cursor.

Based on the MacOS features for adding accented letters with a long keypress.

## Usage

Position the cursor on the character to transform and press `C-x C-a`.

- Select the accented character from the popup with `Enter`
- Close the popup with `C-g`

If the character has no accents available, a message will be prompted accordingly.

### Screenshots

![screen](etc/img/screen.png)

## Config 

``` emacs-lisp
;; Use the character before the cursor instead of after
(setq accent-position 'before)
```

---

Copyright (C) 2022 Elia Scotto
