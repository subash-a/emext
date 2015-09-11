(defun camel-case-text (text &optional separator)
  "convert a given text to camel case, if no seprator is provided assume space as the separator"
  (setq utext (upcase-initials text))
  (setq atext (split-string utext separator))
  (apply 'concat atext))

;; Example conversion to camel case
;; (camel-case-text "subhash_sharma" "_")

(defun split-lines-in-region (begin end)
  (split-string (buffer-substring-no-properties begin end) "\n"))

(defun camelize-lines (lines)
   (map 'list (lambda (text) (camel-case-text text "_")) lines))
;; (camelize-lines '("being_human" "is_the" "only_way"))

(defun join-using-newlines (string-array)
  (reduce (lambda (initial value) (concat initial "\n" value)) string-array))
;; (join-lines '("bring" "it" "on"))

(defun write-to-region (begin end string)
  (save-excursion
	(delete-region begin end)
	(goto-char begin)
	(insert string)))

(defun camel-case-region ()
  (interactive)
  "camel case the string found in the entire region"
  (let ((begin (region-beginning))
		(end (region-end)))
	(write-to-region begin end (join-using-newlines (camelize-lines (split-lines-in-region begin end))))))

;; Example to test the same thing
;; subhash_sharma
;; ashwin_sharma
;; super_man
