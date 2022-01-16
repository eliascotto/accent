;;; accent.el --- Popup for accented characters -*- lexical-binding: t; -*-

;; Copyright (c) 2022 Elia Scotto

;; Author: Elia Scotto <eliascotto94@gmail.com>
;; URL: https://github.com/elias94/accent
;; Keywords: i18n
;; Version: 1.1
;; Package-Requires: ((emacs "24.1") (popup "0.5.8"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; accent.el enable a visual popup for using accented characters in Emacs.
;; See README.md for more information.

;;; Code:

(require 'popup)

(defconst accent-version "1.1"
  "Version of accent.el.")

(defgroup accent nil
  "Shows popup with accented letters while pressing C-x C-a on an accented character."
  :group 'convenience)

(defcustom accent-position 'after
  "If set to 'after (default) it takes the character after (under) the
  cursor, if set to 'before it takes the caracter before the cursor.
  Set it to 'before if you want to set the accent at the end of the typing."
  :group 'accent
  :type 'symbol)

(defvar accent-diacritics nil
  "Diacritics available.")

(setq accent-diacritics '((a (à á â ä æ ã å ā))
                          (c (ç ć č))
                          (e (è é ê ë ē ė ę))
                          (i (î ï í ī į ì))
                          (l (ł))
                          (n (ñ ń))
                          (o (ô ö ò ó œ ø ō õ))
                          (s (ß ś š))
                          (u (û ü ù ú ū))
                          (y (ÿ))
                          (z (ž ź ż))
                          (A (À Á Â Ä Æ Ã Å Ā))
                          (C (Ç Ć Č))
                          (E (È É Ê Ë Ē Ė Ę))
                          (I (Î Ï Í Ī Į Ì))
                          (L (Ł))
                          (N (Ñ Ń))
                          (O (Ô Ö Ò Ó Œ Ø Ō Õ))
                          (S (Ś Š))
                          (U (Û Ü Ù Ú Ū))
                          (Y (Ÿ))
                          (Z (Ž Ź Ż))))

;;;###autoload
(defun accent-menu ()
  "Display a popup menu with available accents if current character is matching."
  (interactive)
  (let* ((after? (eq accent-position 'after))
         (char (if after? (char-after) (char-before)))
         (curr (intern (string char)))
         (diac (assoc curr accent-diacritics)))
    (if diac
        (let ((opt (popup-menu* (cadr diac))))
          (when opt
            (progn
              (delete-char (if after? 1 -1))
              (insert (symbol-name opt)))))
      (message "No accented characters available"))))

(global-set-key (kbd "C-x C-a") 'accent-menu)

(provide 'accent)
;;; accent.el ends here
