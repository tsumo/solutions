(defun atomprint (exp &optional (depth 0))
  "Print each atom in exp, along with its depth of nesting."
  (if (atom exp)
      (format t "~&ATOM: ~a, DEPTH ~d" exp depth)
      (dolist (element exp)
        (atomprint element (+ depth 1)))))

;;; Define a version of last-name that handles "Rex Morgan MD", "Morton
;;; Downey, Jr" and whatever other cases you can think of.

(defun last-name (name)
  (if (member (first (last name))
              '(md jr esquire))
      (last-name (reverse (rest (reverse name))))
      (first (last name))))

(assert (eq (last-name '(rex morgan md)) 'morgan))
(assert (eq (last-name '(morton downey jr)) 'downey))
(assert (eq (last-name '(jack fulton jr esquire)) 'fulton))

;;; Write a function to exponentiate, or raise a number to an integer power.
;;; For example: (power 3 2) = 3^2 = 9

(defun power (x y)
  (cond ((> y 1) (power (* x x) (- y 1)))
        ((= y 1) x)
        ((= y 0) 1)
        (t (error "No negative powers"))))

(assert (= (power 3 2) 9))
(assert (= (power -4 0) 1))

;;; Write a function that counts the number of atoms in an expression. For
;;; example: (count-atoms '(a (b) c)) = 3. Notice that there is something of an
;;; ambiguity in this: should (a nil c) count as three atoms, or as two,
;;; because it is equivalent to (a () c)?

(defun count-atoms (exp)
  (cond ((null exp) 0)
        ((atom (first exp))
         (+ (count-atoms (rest exp)) 1))
        (t (+ (count-atoms (first exp))
              (count-atoms (rest exp))))))

(assert (= (count-atoms '(a nil c)) 3))
(assert (= (count-atoms '(a (v ((n)) b) (c))) 5))

;;; Write a function that count the number of times an expression occurs
;;; anywhere within another expression.
;;; Example: (count-anywhere 'a '(a ((a) b) a)) = 3

(defun count-anywhere (a exp)
  (cond ((null exp) 0)
        ((atom exp) (if (eq a exp)
                        1 0))
        (t (+ (count-anywhere a (first exp))
              (count-anywhere a (rest exp))))))

(assert (= (count-anywhere 'a '(a ((a) b) a)) 3))

;;; Write a function to compute a dot product of two sequences of numbers,
;;; represented as lists. The dot product is computed by multiplying
;;; corresponding elements and then adding up the resulting products.
;;; Example: (dot-product '(10 20) '(3 4)) = 10*3 + 20*4 = 110

(defun dot-product (list-a list-b)
  (apply #'+ (mapcar #'* list-a list-b)))

(assert (= (dot-product '(10 20) '(3 4)) 110))

