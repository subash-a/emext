(defun jshint ()
  "Calls the jshint node application to lint a JS file"
  (interactive)
  (with-output-to-temp-buffer
      "*jshint*" (call-process "jshint" nil "*jshint*" t (buffer-name))))

  
