(load "utils.lisp")

(defun find-n-occurances (str n)
  "Returns 1 if str contains character that appears n times,
   otherwise returns 0."
  (loop for c
        across str
        do (if (= n (count c str)) (return 1)))
  0)

(defun find-twos (str)
  (count-chars str 2))

(defun find-threes (str)
  (count-chars str 3))

(defun day2 ()
  (let ((input (file-to-list "day2.input")))
    (* (reduce #'+ (map 'list #'find-twos input))
       (reduce #'+ (map 'list #'find-threes input)))))

