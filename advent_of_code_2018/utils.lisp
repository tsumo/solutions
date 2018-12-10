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

(defun split (str)
  "Returns a list of substrings of string
   divided by ONE space each.
   Note: Two consecutive spaces will be seen as
   if there were an empty string between them."
  (loop for i = 0 then (1+ j)
        as j = (position #\Space str :start i)
        collect (subseq str i j)
        while j))

(defun range (max &key (min 0) (step 1))
  (loop for n from min below max by step
        collect n))

