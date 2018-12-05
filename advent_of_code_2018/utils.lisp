(defun file-to-list (filename)
  "Reads lines from a file, accumulates them in a list."
  (with-open-file (s filename)
    (do ((l (read-line s) (read-line s nil 'eof))
         (lst nil (append lst (list l))))
        ((eq l 'eof) lst))))

(defun flatten (l)
  "Unnests lists."
  (cond ((null l) nil)
        ((atom (car l)) (cons (car l) (flatten (cdr l))))
        (t (append (flatten (car l)) (flatten (cdr l))))))

