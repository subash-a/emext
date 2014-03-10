;; This utility calculates the hexadecimal value of a given rgb color code

(defun rgb-hex (r g b)
  "Returns the Hexadecimal value of a RGB color code"
  (interactive "nRed: \nnGreen: \nnBlue:")
  (message "Hex value: #%0x%0x%0x" r g b))
