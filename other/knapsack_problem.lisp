(defun knapsack_problem (bag total items)
  "Solves knapsack problem.
   bag - ammount of space left
   total - cost of items in a bag
   items - alist of remaining weight-cost pairs"
  ; (format t "bag ~a~%total ~a~%items ~a~%~%" bag total items)
  (let ((item (car items)))
    (cond ((< bag 0) 0)
          ((null items) total)
          (t (max (knapsack_problem (- bag (car item))
                                    (+ total (cdr item))
                                    (cdr items))
                  (knapsack_problem bag total (cdr items)))))))

;;; Example use:

;;; CL-USER> (knapsack_problem 10 0 '((5 . 10) (4 . 40) (6 . 30) (3 . 50)))
;;; 90

;;; CL-USER> (knapsack_problem 5 0 '((3 . 100) (2 . 20) (4 . 60) (1 . 40)))
;;; 140

