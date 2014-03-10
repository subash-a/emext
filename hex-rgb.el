;; This utility calculates a rgb color code value for a given hexadecimal color code

(defun hex-rgb (h)
  "Returns the RGB value of a Hexadecimal Color Code"
  (interactive "sHex Value:")
  (defun extract-hex (hex)
    "Returns the hexadecimal portion of  string excluding the # symbol"
    (car (cdr (split-string hex "#"))))
  (set 'nhex (extract-hex h))
  (message "Red: %s Green: %s Blue: %s" (string-to-number (substring nhex 0 2) 16) (string-to-number (substring nhex 2 4) 16) (string-to-number (substring nhex 4 6) 16)))
