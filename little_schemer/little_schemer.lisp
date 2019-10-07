;;;; Toys
;;;; Chapter 1

;;; ╔═════════════════════════════════╗
;;; ║        The Law of Car           ║
;;; ║                                 ║
;;; ║ The primitive car is defined    ║
;;; ║ only for non-empty lists.       ║
;;; ║ (Only for lists in Common Lisp) ║
;;; ╚═════════════════════════════════╝

;;; ╔══════════════════════════════════╗
;;; ║        The Law of Cdr            ║
;;; ║                                  ║
;;; ║ The primitive cdr is defined     ║
;;; ║ only for non-empty lists.        ║
;;; ║ (Only for lists in Common Lisp)  ║
;;; ║                                  ║
;;; ║ The cdr of any non-empty list is ║
;;; ║ always another list.             ║
;;; ╚══════════════════════════════════╝

;;; ╔═════════════════════════════════════════╗
;;; ║             The Law of Cons             ║
;;; ║                                         ║
;;; ║ The primitive cons takes two arguments. ║
;;; ║ The second argument to cons must be a   ║
;;; ║ list. The result is a list.             ║
;;; ║ (cons of two atoms is a dot pair)       ║
;;; ╚═════════════════════════════════════════╝

(defun null? (x)
  (null x))

;;; ╔════════════════════════════╗
;;; ║      The Law of Null?      ║
;;; ║                            ║
;;; ║ The primitive null? is de- ║
;;; ║ fined only for lists.      ║
;;; ║ (nil for everything except ║
;;; ║ the empty list)            ║
;;; ╚════════════════════════════╝

(defun atom? (x)
  (not (listp x)))

(defun eq? (x y)
  (eq x y))

;;; ╔═════════════════════════════════╗
;;; ║         The Law of Eq?          ║
;;; ║                                 ║
;;; ║ The primitive eq? takes two ar- ║
;;; ║ guments. Each must be a non-    ║
;;; ║ numeric atom.                   ║
;;; ║ (in practice lists and numbers  ║
;;; ║ can be arguments)               ║
;;; ╚═════════════════════════════════╝

;;;; Do It, Do It Again, and Again, and Again...
;;;; Chapter 2

(defun lat? (l)
  "List of atoms?"
  (cond ((null? l) t)
        ((atom? (car l)) (lat? (cdr l)))
        (t nil)))

(defun member? (a lat)
  "Is there an a inside lat?"
  (cond ((null? lat) nil)
        (t (or (eq? (car lat) a)
               (member? a (cdr lat))))))

;;; ╔════════════════════════════════════════╗
;;; ║          The First Commandment         ║
;;; ║              (preliminary)             ║
;;; ║                                        ║
;;; ║ Always ask null? as the first question ║
;;; ║ in expressing any function.            ║
;;; ╚════════════════════════════════════════╝

;;;; Cons the Magnificent
;;;; Chapter 3

(defun rember (a lat)
  "Remove the first occurence of a in lat."
  (cond ((null? lat) nil)
        ((eq? (car lat) a) (cdr lat))
        (t (cons (car lat) (rember a (cdr lat))))))

;;; ╔══════════════════════════╗
;;; ║  The Second Commandment  ║
;;; ║                          ║
;;; ║ Use cons to build lists. ║
;;; ╚══════════════════════════╝

(defun firsts (l)
  "First S-exp of each internal list of l."
  (cond ((null? l) nil)
        (t (cons (car (car l))
                 (firsts (cdr l))))))

;;; ╔══════════════════════════════════════════╗
;;; ║           The Third Commandment          ║
;;; ║                                          ║
;;; ║ When building a list, describe the first ║
;;; ║ typical element, and then cons it onto   ║
;;; ║ the natural recursion.                   ║
;;; ╚══════════════════════════════════════════╝

(defun insertR (new old lat)
  "Inserts new to the right of the first occurence
   of old in lat."
  (cond ((null? lat) nil)
        ((eq? (car lat) old) (cons old
                                   (cons new
                                         (cdr lat))))
        (t (cons (car lat)
                 (insertR new old (cdr lat))))))

(defun insertL (new old lat)
  "Inserts new to the left of the first occurence
   of old in lat."
  (cond ((null? lat) nil)
        ((eq? (car lat) old) (cons new lat))
        (t (cons (car lat)
                 (insertL new old (cdr lat))))))

(defun subst1 (new old lat)
  "Replaces the first occurence of old with new."
  (cond ((null? lat) nil)
        ((eq? (car lat) old) (cons new (cdr lat)))
        (t (cons (car lat)
                 (subst1 new old (cdr lat))))))

(defun subst2 (new o1 o2 lat)
  "Replaces either the first occurence of o1
   or the first occurence of o2 by new."
  (cond ((null? lat) nil)
        ((or (eq? (car lat) o1)
             (eq? (car lat) o2)) (cons new (cdr lat)))
        (t (cons (car lat)
                 (subst2 new o1 o2 (cdr lat))))))

(defun multirember (a lat)
  "Removes all occurences of a in lat."
  (cond ((null? lat) nil)
        ((eq? (car lat) a) (multirember a (cdr lat)))
        (t (cons (car lat)
                 (multirember a (cdr lat))))))

(defun multiinsertR (new old lat)
  "Inserts new to the right of the every occurence
   of old in lat."
  (cond ((null? lat) nil)
        ((eq? (car lat) old)
         (cons old
               (cons new
                     (multiinsertR new old (cdr lat)))))
        (t (cons (car lat)
                 (multiinsertR new old (cdr lat))))))

(defun multiinsertL (new old lat)
  "Inserts new to the left of the every occurence
   of old in lat."
  (cond ((null? lat) nil)
        ((eq? (car lat) old)
         (cons new
               (cons old
                     (multiinsertL new old (cdr lat)))))
        (t (cons (car lat)
                 (multiinsertL new old (cdr lat))))))

;;; ╔═══════════════════════════════════════════════╗
;;; ║           The Fourth Commandment              ║
;;; ║                (preliminary)                  ║
;;; ║                                               ║
;;; ║ Always change at least one argument while     ║
;;; ║ recurring. It must be changed to be closer to ║
;;; ║ termination. The changing argument must be    ║
;;; ║ tested in the termination condition:          ║
;;; ║ when using cdr, test termination with null?.  ║
;;; ╚═══════════════════════════════════════════════╝

(defun multisubst (new old lat)
  "Replaces all occurences of old with new."
  (cond ((null? lat) nil)
        ((eq? (car lat) old)
         (cons new
               (multisubst new old (cdr lat))))
        (t (cons (car lat)
                 (multisubst new old (cdr lat))))))

;;;; Numbers Games
;;;; Chapter 4

(defun add1 (n)
  (1+ n))

(defun sub1 (n)
  (1- n))

(defun zero? (n)
  (zerop n))

(defun add (n m)
  (cond ((zero? m) n)
        (t (add (add1 n) (sub1 m)))))

(defun sub (n m)
  (cond ((zero? m) n)
        (t (sub1 (sub n (sub1 m))))))

;;; ╔═════════════════════════════════════════════════╗
;;; ║             The First Commandment               ║
;;; ║               (first revision)                  ║
;;; ║                                                 ║
;;; ║ When recurring on a list of atoms, lat, ask two ║
;;; ║ questions about it: (null? lat) and else.       ║
;;; ║                                                 ║
;;; ║ When recurring on a number, n, ask two          ║
;;; ║ questions about it: (zero? n) and else.         ║
;;; ╚═════════════════════════════════════════════════╝

(defun addtup (tup)
  "Adds up all numbers in a tuple (list of numbers)."
  (cond ((null? tup) 0)
        (t (add (car tup)
                (addtup (cdr tup))))))

;;; ╔═══════════════════════════════════════════════╗
;;; ║           The Fourth Commandment              ║
;;; ║              (first revision)                 ║
;;; ║                                               ║
;;; ║ Always change at least one argument while     ║
;;; ║ recurring. It must be changed to be closer to ║
;;; ║ termination. The changing argument must be    ║
;;; ║ tested in the termination condition:          ║
;;; ║ when using cdr, test termination with null?,  ║
;;; ║ when using sub1, test termination with zero?. ║
;;; ╚═══════════════════════════════════════════════╝

(defun mult (n m)
  (cond ((zero? m) 0)
        (t (add n (mult n (sub1 m))))))

;;; ╔═══════════════════════════════════════════════════╗
;;; ║              The Fifth Commandment                ║
;;; ║                                                   ║
;;; ║ When building a value with add, always use 0 for  ║
;;; ║ the value of the terminating line, for adding 0   ║
;;; ║ does not change the value of an addition.         ║
;;; ║                                                   ║
;;; ║ When building a value with mult, always use 1 for ║
;;; ║ the value of the terminating line, for multi-     ║
;;; ║ plying by 1 does not change the value of a        ║
;;; ║ multiplication.                                   ║
;;; ║                                                   ║
;;; ║ When building a value with cons, always consider  ║
;;; ║ () for the value of the terminating line.         ║
;;; ╚═══════════════════════════════════════════════════╝

(defun tup+ (tup1 tup2)
  "Adds together corresponding numbers for tups."
  (cond ((null? tup1) tup2)
        ((null? tup2) tup1)
        (t (cons (add (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2))))))

(defun more (n m)
  "Is n > m?"
  (cond ((zero? n) nil)
        ((zero? m) t)
        (t (more (sub1 n) (sub1 m)))))

(defun less (n m)
  "Is n < m?"
  (cond ((zero? m) nil)
        ((zero? n) t)
        (t (less (sub1 n) (sub1 m)))))

(defun equal-num (n m)
  "Is n = m?"
  (cond ((or (more n m)
             (less n m))
         nil)
        (t t)))

(defun power (n m)
  "Exponentiation."
  (cond ((zero? m) 1)
        (t (mult n (power n (sub1 m))))))

(defun div (n m)
  "Integer division."
  (cond ((less n m) 0)
        (t (add1 (div (sub n m) m)))))

(defun length-lat (lat)
  "Length of lat."
  (cond ((null? lat) 0)
        (t (add1 (length-lat (cdr lat))))))

(defun pick (n lat)
  "Picks nth atom from lat."
  (cond ((zero? (sub1 n)) (car lat))
        (t (pick (sub1 n) (cdr lat)))))

(defun rempick (n lat)
  "Removes nth atom from lat."
  (cond ((zero? (sub1 n)) (cdr lat))
        (t (cons (car lat)
                 (rempick (sub1 n) (cdr lat))))))

(defun number? (a)
  (numberp a))

(defun no-nums (lat)
  "Removes all numbers from lat."
  (cond ((null? lat) nil)
        ((number? (car lat)) (no-nums (cdr lat)))
        (t (cons (car lat)
                 (no-nums (cdr lat))))))

(defun all-nums (lat)
  "Extracts all numbers from lat."
  (cond ((null? lat) nil)
        ((number? (car lat)) (cons (car lat)
                                   (all-nums (cdr lat))))
        (t (all-nums (cdr lat)))))

(defun eqan? (a1 a2)
  "Is a1 and a2 are the same atom?
   Genaralisation for numeric and non-numeric atoms."
  (cond ((and (number? a1) (number? a2))
         (equal-num a1 a2))
        ((or (number? a1) (number? a2))
         nil)
        (t (eq? a1 a2))))

(defun occur (a lat)
  "Counts number of times an a appears in a lat."
  (cond ((null? lat) 0)
        ((eqan? (car lat) a)
         (add1 (occur a (cdr lat))))
        (t (occur a (cdr lat)))))

(defun one? (n)
  (equal-num n 1))

(defun rempick-using-one (n lat)
  "Removes nth atom from lat using one? check."
  (cond ((one? n) (cdr lat))
        (t (cons (car lat)
                 (rempick-using-one (sub1 n)
                                    (cdr lat))))))

;;;; *Oh My Gawd*: It's Full of Stars
;;;; Chapter 5

(defun rember* (a l)
  "Remove all occurences of a in l."
  (cond ((null? l) nil)
        ((atom? (car l))
         (cond ((eqan? (car l) a) (rember* a (cdr l)))
               (t (cons (car l)
                        (rember* a (cdr l))))))
        (t (cons (rember* a (car l))
                 (rember* a (cdr l))))))

(defun insertR* (new old l)
  "Inserts new to the right of the every occurence
   of old in l."
  (cond ((null? l) nil)
        ((atom? (car l))
         (cond ((eqan? (car l) old)
                (cons old
                      (cons new
                            (insertR* new old (cdr l)))))
               (t (cons (car l)
                        (insertR* new old (cdr l))))))
        (t (cons (insertR* new old (car l))
                 (insertR* new old (cdr l))))))

;;; ╔═══════════════════════════════════════════════╗
;;; ║             The First Commandment             ║
;;; ║                (final version)                ║
;;; ║                                               ║
;;; ║ When recurring on a list of atoms, lat, ask   ║
;;; ║ two questions about it: (null? lat) and else. ║
;;; ║                                               ║
;;; ║ When recurring on a number, n, ask two        ║
;;; ║ questions about it: (zero? n) and else.       ║
;;; ║                                               ║
;;; ║ When recurring on a list of S-expressions, l, ║
;;; ║ ask three questions about it: (null? l),      ║
;;; ║ (atom? (car l)), and else.                    ║
;;; ╚═══════════════════════════════════════════════╝

;;; ╔═════════════════════════════════════════════════╗
;;; ║           The Fourth Commandment                ║
;;; ║              (final revision)                   ║
;;; ║                                                 ║
;;; ║ Always change at least one argument while       ║
;;; ║ recurring. When recurring on a list of atoms,   ║
;;; ║ lat, use (cdr lat). When recurring on a number, ║
;;; ║ n, use (sub1 n). And when recurring on a list   ║
;;; ║ of S-expressions, l, use (car l) and (cdr l)    ║
;;; ║ if neither (null? l) nor (atom? (car l)) are    ║
;;; ║ true.                                           ║
;;; ║                                                 ║
;;; ║ It must be changed to be closer to termination. ║
;;; ║ The changing argument must be tested in the     ║
;;; ║ termination condition:                          ║
;;; ║ when using cdr, test termination with null?,    ║
;;; ║ when using sub1, test termination with zero?.   ║
;;; ╚═════════════════════════════════════════════════╝

(defun occur* (a l)
  "Count the number of times a is found in l."
  (cond ((null? l) 0)
        ((atom? (car l))
         (cond ((eqan? (car l) a)
                (add1 (occur* a (cdr l))))
               (t (occur* a (cdr l)))))
        (t (add (occur* a (car l))
                (occur* a (cdr l))))))

(defun subst* (new old l)
  "Replace all occurences of old with new in l."
  (cond ((null? l) nil)
        ((atom? (car l))
         (cond ((eqan? (car l) old)
                (cons new
                      (subst* new old (cdr l))))
               (t (cons (car l)
                        (subst* new old (cdr l))))))
        (t (cons (subst* new old (car l))
                 (subst* new old (cdr l))))))

(defun insertL* (new old l)
  "Inserts new to the left of the every occurence
   of old in l."
  (cond ((null? l) nil)
        ((atom? (car l))
         (cond ((eqan? (car l) old)
                (cons new
                      (cons old
                            (insertL* new old (cdr l)))))
               (t (cons (car l)
                        (insertL* new old (cdr l))))))
        (t (cons (insertL* new old (car l))
                 (insertL* new old (cdr l))))))

(defun member* (a l)
  "Does a appear anywhere in l?"
  (cond ((null? l) nil)
        ((atom? (car l))
         (or (eqan? (car l) a)
             (member* a (cdr l))))
        (t (or (member* a (car l))
               (member* a (cdr l))))))

(defun leftmost (l)
  "Finds the leftmost atom in non-empty l
   that does not contain the empty list."
  (cond ((atom? (car l)) (car l))
        (t (leftmost (car l)))))

;;; (and ...) stops if it encounters false value.
;;; (or ...) stops it it encounters true value.
;;; (cond ...) also has the property of not considering
;;; all of its arguments, which means that neither
;;; (and ...) nor (or ...) can be defined as functions
;;; in terms of (cond ...), though both can be expressed
;;; as abbreviations of (cond ...)-expressions:
;;;     (and A B) = (cond (A B) (t nil))
;;;     (or A B) = (cond (A t) (t B))

(defun equal? (s1 s2)
  "Are s-exps s1 and s2 equal?"
  (cond ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
        ((or (atom? s1) (atom? s2)) nil)
        (t (eqlist? s1 s2))))

(defun eqlist? (l1 l2)
  "Are lists l1 and l2 equal?"
  (cond ((and (null? l1) (null? l2)) t)
        ((or (null? l1) (null? l2)) nil)
        (t (and (equal? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2))))))

;;; ╔══════════════════════════════════════════════╗
;;; ║             The Sixth Commandment            ║
;;; ║                                              ║
;;; ║ Simplify only after the function is correct. ║
;;; ╚══════════════════════════════════════════════╝

(defun rember-simplified (s l)
  "Remove first occurence of s-exp from list."
  (cond ((null? l) nil)
        ((equal? (car l) s) (cdr l))
        (t (cons (car l)
                 (rember s (cdr l))))))

;;;; Shadows
;;;; Chapter 6

(defun numbered? (aexp)
  "Is aexp a representation of arithmetic expression?
   Example: (8 + ((5 ^ 4) * 9))"
  (cond ((atom? aexp) (number? aexp))
        (t (and (numbered? (car aexp))
                (numbered? (car (cdr (cdr aexp))))))))

;;; ╔════════════════════════════════════════════╗
;;; ║            The Seventh Commandment         ║
;;; ║                                            ║
;;; ║ Recur on the subparts that are of the same ║
;;; ║ nature:                                    ║
;;; ║ * On the sublists of a list                ║
;;; ║ * On the subexpressions of an arithmetic   ║
;;; ║   expression.                              ║
;;; ╚════════════════════════════════════════════╝

(defun value (nexp)
  "Compute the value of the arithmetic expression."
  (cond ((atom? nexp) nexp)
        ((eq? (car (cdr nexp)) '+)
         (add (value (car nexp))
              (value (car (cdr (cdr nexp))))))
        ((eq? (car (cdr nexp)) '*)
         (mult (value (car nexp))
               (value (car (cdr (cdr nexp))))))
        (t (power (value (car nexp))
                  (value (car (cdr (cdr nexp))))))))

(defun 1st-sub-exp (aexp)
  "First subexpression of the representation
   of an arithmetic expression in polish notation."
  (car (cdr aexp)))

(defun 2nd-sub-exp (aexp)
  "Second subexpression of the representation
   of an arithmetic expression in polish notation."
  (car (cdr (cdr aexp))))

(defun operator (aexp)
  "Extracts operator from the representation
   of an arithmetic expression in polish notation."
  (car aexp))

(defun value-polish (nexp)
  "Compute the value of the arithmetic expression
   in polish notation."
  (cond ((atom? nexp) nexp)
        ((eq? (operator nexp) '+)
         (add (value-polish (1st-sub-exp nexp))
              (value-polish (2nd-sub-exp nexp))))
        ((eq? (operator nexp) '*)
         (mult (value-polish (1st-sub-exp nexp))
               (value-polish (2nd-sub-exp nexp))))
        (t (power (value-polish (1st-sub-exp nexp))
                  (value-polish (2nd-sub-exp nexp))))))

;;; ╔══════════════════════════════════════════════════════╗
;;; ║                 The Eighth Commandment               ║
;;; ║                                                      ║
;;; ║ Use help functions to abstract from representations. ║
;;; ╚══════════════════════════════════════════════════════╝

(defun sero? (n)
  "Checks for zero in a number representation where
   zero is (), one is (()), two is (() ()) etc."
  (null? n))

(defun edd1 (n)
  (cons nil n))

(defun zub1 (n)
  (cdr n))

(defun edd (n m)
  (cond ((sero? m) n)
        (t (edd (edd1 n)
                (zub1 m)))))

;;;; Friends and Relations
;;;; Chapter 7

(defun set? (lat)
  "Is lat a mathematical set?"
  (cond ((null? lat) t)
        ((member? (car lat) (cdr lat)) nil)
        (t (set? (cdr lat)))))

(defun makeset (lat)
  "Remove duplicates from lat."
  (cond ((null? lat) nil)
        ((member? (car lat) (cdr lat))
         (makeset (cdr lat)))
        (t (cons (car lat)
                 (makeset (cdr lat))))))

(defun makeset-mr (lat)
  "makeset using multirember."
  (cond ((null? lat) nil)
        (t (cons (car lat)
                 (makeset-mr (multirember (car lat)
                                          (cdr lat)))))))

(defun subset? (set1 set2)
  "Is set1 a subset of set2?"
  (cond ((null? set1) t)
        ((member? (car set1) set2)
         (subset? (cdr set1) set2))
        (t nil)))

(defun subset?-and (set1 set2)
  "subset using and."
  (cond ((null? set1) t)
        (t (and (member? (car set1) set2)
                (subset? (cdr set1) set2)))))

(defun eqset? (set1 set2)
  "Are set1 and set2 equal sets?"
  (and (subset? set1 set2) (subset? set2 set1)))

(defun intersect? (set1 set2)
  "Is there at least one atom from set1 in set2?"
  (cond ((null? set1) nil)
        (t (or (member? (car set1) set2)
               (intersect? (cdr set1) set2)))))

(defun intersect (set1 set2)
  "Return atoms that are in both sets."
  (cond ((null? set1) nil)
        ((member? (car set1) set2)
         (cons (car set1) (intersect (cdr set1) set2)))
        (t (intersect (cdr set1) set2))))

(defun union_ (set1 set2)
  "Return set of all atoms of both sets."
  (cond ((null? set1) set2)
        ((member? (car set1) set2) (union_ (cdr set1) set2))
        (t (cons (car set1)
                 (union_ (cdr set1) set2)))))

(defun difference (set1 set2)
  "Return all atoms from set1 that are not in set2."
  (cond ((null? set1) nil)
        ((member (car set1) set2)
         (difference (cdr set1) set2))
        (t (cons (car set1)
                 (difference (cdr set1) set2)))))

(defun intersectall (l-set)
  "intersect for a list of sets."
  (cond ((null? (cdr l-set)) (car l-set))
        (t (intersect (car l-set)
                      (intersectall (cdr l-set))))))

(defun a-pair? (x)
  "Is x a list with only two S-expressions?"
  (cond ((atom? x) nil)
        ((null? x) nil)
        ((null? (cdr x)) nil)
        ((null? (cdr (cdr x))) t)
        (t nil)))

(defun first_ (p)
  "Return first element from a pair."
  (car p))

(defun second_ (p)
  "Return second element from a pair."
  (car (cdr p)))

(defun build (s1 s2)
  "Create a pair from two S-expressions."
  (cons s1 (cons s2 nil)))

(defun third_ (l)
  "Returns third element from a list."
  (car (cdr (cdr l))))

(defun fun? (rel)
  "Is rel a function?
   rel is a relataion - a set of pairs.
   Function cannot have two results for the same argument."
  (set? (firsts rel)))

(defun revpair (p)
  "Reverse a pair."
  (build (second_ p) (first_ p)))

(defun revrel (rel)
  "Reverse a relation."
  (cond ((null? rel) nil)
        (t (cons (revpair (car rel))
                 (revrel (cdr rel))))))

(defun one-to-one? (fun)
  "Is fun a function in which results are a set?"
  (fun? (revrel fun)))

;;;; Lambda the Ultimate
;;;; Chapter 8

(defun eq?-c (a)
  "Curried eq. Returns a function to compare
   anything to a."
  (lambda (x)
    (eq x a)))

;;; Compares any atom to salad
(defparameter eq?-salad (eq?-c 'salad))

(defun rember-f (test?)
  "Returns a function that takes two arguments and
   removes first element a from l that satisfies
   supplied test function.
   Example call:
   (funcall (rember-f #'eq?) 'tuna '(tuna salad is good))"
  (lambda (a l)
    (cond ((null? l) nil)
          ((funcall test? (car l) a) (cdr l))
          (t (cons (car l)
                   (funcall (rember-f test?) a (cdr l)))))))

(defun insertL-f (test?)
  "Returns an insertL-type function that takes two arguments
   and uses supplied test function for equality check.
   Example call:
   (funcall (insertL-f 'eq) 'tuna 'salad '(salad is good))"
  (lambda (new old l)
    (cond ((null? l) nil)
          ((funcall test? (car l) old)
           (cons new (cons old (cdr l))))
          (t (cons (car l)
                   (funcall (insertL-f test?)
                            new old (cdr l)))))))

(defun insertR-f (test?)
  "Returns an insertR-type function that uses supplied
   test function for equality check.
   Example call:
   (funcall (insertR-f 'eq) 'salad 'tuna '(tuna is good))"
  (lambda (new old l)
    (cond ((null? l) nil)
          ((funcall test? (car l) old)
           (cons old (cons new (cdr l))))
          (t (cons (car l)
                   (funcall (insertR-f test?)
                            new old (cdr l)))))))

(defun seqL (new old l)
  "Helper function for insertL."
  (cons new (cons old l)))

(defun seqR (new old l)
  "Helper function for insertR."
  (cons old (cons new l)))

(defun insert-g (seq)
  "Returns a function that can insert new atom to either
   left or right of the old atom, depending on the
   helper function provided.
   Example call:
   (funcall (insert-g #'seqL) 'tuna 'salad '(salad is good))
   (funcall (insert-g #'seqR) 'salad 'tuna '(tuna is good))"
  (lambda (new old l)
    (cond ((null? l) nil)
          ((eq? (car l) old)
           (funcall seq new old (cdr l)))
          (t (cons (car l)
                   (funcall (insert-g seq)
                            new old (cdr l)))))))

;;; Example call:
;;; (funcall insert-gL 'tuna 'salad '(salad is good))
(defparameter insert-gL (insert-g #'seqL))

;;; Example call:
;;; (funcall insert-gR 'salad 'tuna '(tuna is good))
(defparameter insert-gR (insert-g #'seqR))

(defun seqS (new old l)
  "Helper function for subst."
  (declare (ignore old))
  (cons new l))

;;; subst function
;;; Example call:
;;; (funcall insert-gS 'oranges 'apples '(apples are tasty))
(defparameter insert-gS (insert-g #'seqS))

(defun seqrem (new old l)
  "Helper function for rember."
  (declare (ignore new old))
  l)

(defun rember-g (a l)
  "rember using insert-g."
  (funcall (insert-g #'seqrem) nil a l))

;;; ╔═══════════════════════════════════════════════╗
;;; ║              The Ninth Commandment            ║
;;; ║                                               ║
;;; ║ Abstract common patterns with a new function. ║
;;; ╚═══════════════════════════════════════════════╝

(defun atom-to-function (x)
  "Returns appropriate arithmetic function
   based on the argument."
  (cond ((eq? x '+) #'add)
        ((eq? x '*) #'mult)
        (t #'power)))

(defun value-l (nexp)
  "Computes the value of arithmetic expressions,
   like (+ 2 (* 3 2))."
  (cond ((atom? nexp) nexp)
        (t (funcall (atom-to-function (operator nexp))
                    (value-l (1st-sub-exp nexp))
                    (value-l (2nd-sub-exp nexp))))))

(defun multirember-f (test?)
  "Takes a test function and returns a function that
   uses this test function to filter lat from
   elements a."
  (lambda (a lat)
    (cond ((null? lat) nil)
          ((funcall test? (car lat) a)
           (funcall (multirember-f test?)
                    a
                    (cdr lat)))
          (t (cons (car lat)
                   (funcall (multirember-f test?)
                            a
                            (cdr lat)))))))

;;; Returns a function to compare anything to tuna.
(defparameter eq?-tuna (eq?-c 'tuna))

(defun multiremberT (test? lat)
  "Takes a test function and uses it to filter our
   elements from lat.
   Example call:
   (multiremberT eq?-tuna '(tuna salad and tuna sandwich))"
  (cond ((null? lat) nil)
        ((funcall test? (car lat))
         (multiremberT test? (cdr lat)))
        (t (cons (car lat)
                 (multiremberT test? (cdr lat))))))

(defun a-friend (x y)
  "Asks whether multirember&co found a in lat or not."
  (declare (ignore x))
  (null? y))

(defun new-friend (newlat seen)
  "Shows what happens when multirember&co founds a in lat.
   Second branch of cond.
   Value gets consed into the second list."
  (a-friend newlat (cons 'tuna seen)))

(defun latest-friend (newlat seen)
  "Shows what happens when miltirember&co doesn't find a in lat.
   Third branch of cond.
   Value gets consed into the first list."
  (a-friend (cons 'and newlat) seen))

(defun last-friend (x y)
  "Asks how many not-a's multirember&co had found in lat."
  (declare (ignore y))
  (length-lat x))

(defun multirember&co (a lat col)
  "Checks each atom in lat for equality to a.
   If atom is equal it gets stored in the second argument to col.
   If atom is not equal it gets stored in the first argument to col.
   At the end original col passed to the function will be called
   on the collected values.
   Example call:
   (multirember&co 'tuna '(strawberries tuna and swordfish) #'last-friend)"
  (cond ((null? lat)
         (funcall col nil nil))
        ((eq? (car lat) a)
         (multirember&co a
                         (cdr lat)
                         (lambda (newlat seen)
                           (funcall col
                                    newlat
                                    (cons (car lat)
                                          seen)))))
        (t (multirember&co a
                           (cdr lat)
                           (lambda (newlat seen)
                             (funcall col
                                      (cons (car lat)
                                            newlat)
                                      seen))))))

;;; ╔═════════════════════════════════╗
;;; ║       The Tenth Commandment     ║
;;; ║                                 ║
;;; ║ Build functions to collect more ║
;;; ║ than one value at a time.       ║
;;; ╚═════════════════════════════════╝

(defun multiinsertLR (new oldL oldR lat)
  "Inserts new to the left of oldL and to the right
   of the oldR in lat.
   oldL and oldR should be different for the function to
   work as expected."
  (cond ((null? lat) nil)
        ((eq? oldL (car lat))
         (cons new
               (cons oldL
                     (multiinsertLR new oldL oldR (cdr lat)))))
        ((eq? oldR (car lat))
         (cons oldR
               (cons new
                     (multiinsertLR new oldL oldR (cdr lat)))))
        (t (cons (car lat)
                 (multiinsertLR new oldL oldR (cdr lat))))))

(defun multiinsertLR&co (new oldL oldR lat col)
  "multiinsertLR in continuation-passing style.
   Remembers number of inserts to left and right.
   Example call:
   (multiinsertLR&co 'new 'oldL 'oldR '(test test oldL test test oldR test test) #'list)"
  (cond ((null? lat) (funcall col nil 0 0))
        ((eq? oldL (car lat))
         (multiinsertLR&co new oldL oldR
                           (cdr lat)
                           (lambda (newlat L R)
                             (funcall col
                                      (cons new
                                            (cons oldL
                                                  newlat))
                                      (add1 L)
                                      R))))
        ((eq? oldR (car lat))
         (multiinsertLR&co new oldL oldR
                           (cdr lat)
                           (lambda (newlat L R)
                             (funcall col
                                      (cons oldR
                                            (cons new
                                                  newlat))
                                      L
                                      (add1 R)))))
        (t (multiinsertLR&co new oldL oldR
                             (cdr lat)
                             (lambda (newlat L R)
                               (funcall col
                                        (cons (car lat)
                                              newlat)
                                        L
                                        R))))))

(defun even? (n)
  "Check number for evennes using integer division."
  (equal-num (mult (div n 2) 2) n))

(defun evens-only* (l)
  "Filters out all odd numbers."
  (cond ((null? l) nil)
        ((atom? (car l))
         (cond ((even? (car l))
                (cons (car l)
                      (evens-only* (cdr l))))
               (t (evens-only* (cdr l)))))
        (t (cons (evens-only* (car l))
                 (evens-only* (cdr l))))))

(defun the-last-friend (newl product sum)
  "Collector for the evens-only*&co."
  (cons sum (cons product newl)))

(defun evens-only*&co (l col)
  "Filters out odd numbers, while multiplying all evens
   and summing all odds.
   Example call:
   (evens-only*&co '((9 1 2 8) 3 10 ((9 9)7 6) 2) #'the-last-friend)"
  (cond ((null? l) (funcall col nil 1 0))
        ((atom? (car l))
         (cond ((even? (car l))
                (evens-only*&co
                  (cdr l)
                  (lambda (newl p s)
                    (funcall col
                             (cons (car l) newl)
                             (mult (car l) p)
                             s))))
               (t (evens-only*&co
                    (cdr l)
                    (lambda (newl p s)
                      (funcall col
                               newl
                               p
                               (add (car l) s)))))))
        (t (evens-only*&co
             (car l)
             (lambda (al ap as)
               (evens-only*&co
                 (cdr l)
                 (lambda (dl dp ds)
                   (funcall col
                            (cons al dl)
                            (mult ap dp)
                            (add as ds)))))))))

;;;; ...and Again, and Again, and Again,...
;;;; Chapter 9

(defun keep-looking (a sorn lat)
  "Helper function for looking.
   sorn means symbol or number."
  (cond ((number? sorn)
         (keep-looking a (pick sorn lat) lat))
        (t (eq? sorn a))))

(defun looking (a lat)
  "Looks for a in lat starting from the first atom.
   If this atom is a number - check at that position in lat.
   If it is a symbol - check if it is a."
  (keep-looking a (pick 1 lat) lat))

(defun eternity (x)
  "Partial function that never reaches it's goal."
  (eternity x))

(defun shift (pair)
  "Takes a pair whose first component is a pair,
   and shifts its second part into the second component."
  (build (first_ (first_ pair))
         (build (second_ (first_ pair))
                (second_ pair))))

(defun align (pora)
  "Shifts to the right elements of a pair,
   each element could also be a pair.
   pora means pair or atom.
   Total function - yields a value for every argument.
   Example call:
   (align '(2 ((3 4) 3)))"
  (cond ((atom? pora) pora)
        ((a-pair? (first_ pora))
         (align (shift pora)))
        (t (build (first_ pora)
                  (align (second_ pora))))))

(defun length* (pora)
  "Finds number of atoms in pair of atoms or pairs."
  (cond ((atom? pora) 1)
        (t (add (length* (first_ pora))
                (length* (second_ pora))))))

(defun weight* (pora)
  "Calculates how un-aligned argument to align is."
  (cond ((atom? pora) 1)
        (t (add (mult (weight* (first_ pora)) 2)
                (weight* (second_ pora))))))

(defun shuffle (pora)
  "Shuffles elements of a pair if the first component
   is a pair.
   Partial function - enters an infinite loop
   when both components are pairs."
  (cond ((atom? pora) pora)
        ((a-pair? (first_ pora))
         (shuffle (revpair pora)))
        (t (build (first_ pora)
                  (shuffle (second_ pora))))))

(defun C (n)
  "Collatz conjecture. Probably a total function."
  (cond ((one? n) 1)
        (t (cond ((even? n) (C (div n 2)))
                 (t (C (add1 (mult 3 n))))))))

(defun A (n m)
  "Ackermann function. A total function."
  (cond ((zero? n) (add1 m))
        ((zero? m) (A (sub1 n) 1))
        (t (A (sub1 n)
              (A n (sub1 m))))))

(defun will-stop? (f)
  "Hypothetical function that checks if the function stops
   when it's called with nil as the argument."
  (declare (ignore f)))

(defun last-try (x)
  "Function that breaks will-stop?.
   Thank you, Alan M. Turing and Kurt Godel."
  (and (will-stop? #'last-try)
       (eternity x)))

;;; length 0
;;; Function that determines the length of
;;; the empty list and nothing else.
(lambda (l)
  (cond ((null? l) 0)
        (t (add1
             (eternity (cdr l))))))

;;; length <=1
;;; Contains definition of length 0 in last
;;; cond clause, since length 0 wasn't defined.
(lambda (l)
  (cond ((null? l) 0)
        (t (add1
             ((lambda (l)
                (cond ((null? l) 0)
                      (t (add1
                           (eternity (cdr l))))))
              (cdr l))))))

;;; length <=2
;;; eternity just gets replaced with the
;;; next version of length.
(lambda (l)
  (cond ((null? l) 0)
        (t (add1
             ((lambda (l)
                (cond ((null? l) 0)
                      (t (add1
                           ((lambda (l)
                              (cond ((null? l) 0)
                                    (t (add1
                                         (eternity (cdr l))))))
                            (cdr l))))))
              (cdr l))))))

;;; Abstracting away length function
;;; This one creates length 0
((lambda (length_)
   (lambda (l)
     (cond ((null? l) 0)
           (t (add1 (funcall length_ (cdr l)))))))
 #'eternity)

;;; length <=1 in the same style.
((lambda (f)
   (lambda (l)
     (cond ((null? l) 0)
           (t (add 1 (funcall f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond ((null? l) 0)
            (t (add1 (funcall g (cdr l)))))))
  #'eternity))

;;; length <=2
((lambda (length_)
   (lambda (l)
     (cond ((null? l) 0)
           (t (add1 (funcall length_ (cdr l)))))))
 ((lambda (length_)
    (lambda (l)
      (cond ((null? l) 0)
            (t (add1 (funcall length_ (cdr l)))))))
  ((lambda (length_)
     (lambda (l)
       (cond ((null? l) 0)
             (t (add1 (funcall length_ (cdr l)))))))
   #'eternity)))

;;; Abstract away a function that takes length as an
;;; argument and returns a function that looks like length.
;;; length 0
((lambda (mk-length)
   (funcall mk-length #'eternity))
 (lambda (length_)
   (lambda (l)
     (cond ((null? l) 0)
           (t (add1 (funcall length_ (cdr l))))))))

;;; length <=1
((lambda (mk-length)
   (funcall mk-length
     (funcall mk-length #'eternity)))
 (lambda (length_)
   (lambda (l)
     (cond ((null? l) 0)
           (t (add1 (funcall length_ (cdr l))))))))

;;; length <=2
((lambda (mk-length)
   (funcall mk-length
     (funcall mk-length
       (funcall mk-length #'eternity))))
 (lambda (length_)
   (lambda (l)
     (cond ((null? l) 0)
           (t (add1 (funcall length_ (cdr l))))))))

;;; Still length 0
((lambda (mk-length)
   (funcall mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
           (t (add1 (funcall mk-length (cdr l))))))))

;;; Here is length function, finally.
((lambda (mk-length)
   (funcall mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
           (t (add1 ((lambda (x) (funcall
                                   (funcall mk-length mk-length) x))
                     (cdr l))))))))

(lambda (le)
  ((lambda (mk-length)
     (funcall mk-length mk-length))
   (lambda (mk-length)
     (funcall le (lambda (x)
                   (funcall (funcall mk-length mk-length) x))))))

(defun Y (le)
  ((lambda (f) (funcall f f))
   (lambda (f)
     (funcall le (lambda (x) (funcall (funcall f f) x))))))

;;; Applying Y to a function to get a recursion
(funcall (Y (lambda (length)
              (lambda (l)
                (cond ((null? l) 0)
                      (t (add1 (funcall length (cdr l))))))))
         '(1 2 3 4 5))

;;;; What Is the Value of All of This?
;;;; Chapter 10

(defun new-entry (a b)
  "Entry is a pair whose first list is a set.
   Lists must be of equal length.
   Examples:
     ((appetizer entree beverage)
      (pate boeuf vin))
   and
     ((beverage dessert)
      (beer beer))"
  (build a b))

