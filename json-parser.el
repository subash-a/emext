;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(setq json "{key:val}")


(defun regexp-equal (token regexp)
"This function returns true or false based on the token being equal 
to regular expression. the token value is compared to the regular expression
using a string-match function and then if there is non-nil value returned
then a non-nil value is returned else nil is returned."
(if (string-match regexp token) t nil ))

(string-match "[a-z]" "0")
;;Test cases for regexp-equal
(regexp-equal "0" "[a-z]")

(defun check-char (token)
  (regexp-equal (char-to-string token) "[a-z]"))
;;Test case for check-char function
(check-char 123)
(check-char 107)

(defun check-openbrace (token)
  (string-equal "{" (char-to-string token)))

(defun check-closebrace (token)
  (string-equal "}" (char-to-string token)))

(defun check-separator (token)
  "This function returns true if the token character is a key value separator
This checks for the : separator which separates key from value in JSON."
  (string-equal ":" (char-to-string token)))

;;The function returns the new state based on previous state the logic is
;; if previous state is brace open then its the beginning of new key
;; if previous state is end of key i.e spearator then value begins
;; if previous state is object separator i.e comma then new key begins
(defun check-prev-state (state)
"This function returns the new state based on previous state of parser.
The logic is as described:
Previous State --> Next State
BROPEN --> KEYBEG
KEYEND --> VALBEG
NXTOBJ --> KEYBEG
Any other state --> ERROR"
  (cond ((string-equal state "OBJBEG") (setq response "KEYBEG"))
	((string-equal state "KEYEND") (setq response "VALBEG"))
	((string-equal state "NXTOBJ") (setq response "KEYBEG"))
	((string-equal state "KEYBEG") (setq response "KEY"))
	((string-equal state "KEY") (setq response "KEY"))
	((string-equal state "VALBEG") (setq response "VAL"))
	((string-equal state "VAL") (setq response "VAL"))
	(t (setq response "ERROR"))))

;;Test Cases for check-prev-state function
(check-prev-state "BROPEN")
(check-prev-state "KEYEND")
(check-prev-state "NXTOBJ")
(check-prev-state "BRCLOSE")
;; End of Test Cases

(defun get-object ()
(make-hash-table))

(defun show-table-contents (table)
  (setq keys "" values "")
  (maphash (lambda (key value) (setq keys (concat keys "," key)
				   values (concat values "," value))) table)
  keys
  values)


(defun make-object (json)
"This function extracts the JSON objects from a string"
;; Variable initialization for storing json object
(setq literal "" 
      state "" 
      result "" 
      tokens (string-to-list json) 
      obj "" 
      key ""
      value "")
;; iterate through the tokens list and return an object
(dolist (token tokens result)
;; Checking conditions for object beginning, end, string beginning, 
;; value beginning and end. And appropriate functions get called.
  (cond 
   ((check-openbrace token) (setq literal (concat literal "BROPEN") 
				  state "OBJBEG")
    (setq obj (get-object)))
   ((check-closebrace token) (setq literal (concat literal "BRCLOSE") 
				   state "OBJEND")
    (puthash key value obj))
   ((check-separator token) (setq literal (concat literal "SEP") 
				  state "KEYEND"))
   ((check-char token) (setq literal (concat literal "CHAR")) 
    (setq state (check-prev-state state))
;; The below if condition checks for the state of KEYBEG or KEY and then
;; updates the key variable to the concatenated key string.
    (if (or (string-equal state "KEYBEG") (string-equal state "KEY"))
	(setq key (concat key (char-to-string token))))
;; The below if condition checks for the state of VALBEG or VAL and then
;; updates the value variable to the concated value string.
    (if (or (string-equal state "VALBEG") (string-equal state "VAL"))
	(setq value (concat value (char-to-string token))))))
  (setq result obj)))

(show-table-contents (make-object json))
