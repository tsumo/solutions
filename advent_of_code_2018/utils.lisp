(defun file-to-list (filename)
  "Reads lines from a file, accumulates them in a list."
  (with-open-file (s filename)
    (do ((l (read-line s) (read-line s nil 'eof))
         (lst nil (append lst (list l))))
        ((eq l 'eof) lst))))

