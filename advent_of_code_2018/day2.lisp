(load "utils.lisp")

(defun find-n-occurrences (str n)
  "Returns 1 if str contains character that appears n times,
   otherwise returns 0."
  (loop for c
        across str
        do (if (= n (count c str)) (return-from find-n-occurrences 1)))
  0)

(defun find-twos (str)
  (find-n-occurrences str 2))

(defun find-threes (str)
  (find-n-occurrences str 3))

(defun str-diff (s1 s2)
  "Finds how many characters are different in the
   two strings of equal length."
  (let ((diff 0))
    (loop for c1 across s1
          for c2 across s2
          do (if (not (eql c1 c2))
                 (incf diff)))
    diff))

(defun find-similar-ids (lst n)
  "Finds two strings in a list with difference
   of n characters."
  (loop for s1 in lst
        do (loop for s2 in lst
                 do (if (= (str-diff s1 s2) n)
                        (return-from find-similar-ids (list s1 s2)))))
  nil)

(defun find-common-characters (s1 s2)
  "Compares each letter from s1 to the corresponding letter from s2,
   and filters them out when they are not equal."
  (let ((common-characters nil))
    (loop for c1 across s1
          for c2 across s2
          do (if (eql c1 c2)
                 (setf common-characters (append common-characters (list c1)))))
    (concatenate 'string common-characters)))

(defun day2 ()
  (let* ((input (file-to-list "day2.input"))
         (checksum (* (reduce #'+ (map 'list #'find-twos input))
                      (reduce #'+ (map 'list #'find-threes input))))
         (similar-ids (find-similar-ids input 1))
         (common-characters (apply #'find-common-characters similar-ids)))
    (format t "Checksum: ~a~%" checksum)
    (format t "Similar ids: ~a and ~a~%" (car similar-ids) (cadr similar-ids))
    (format t "Common characters: ~a" common-characters)))

