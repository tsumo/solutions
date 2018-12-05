(load "utils.lisp")

(defstruct claim
  id x y width height)

(defun parse-claim-string (str)
  "Creates struct from a claim string."
  (let* ((id-start 1)
         (id-end (- (position #\@ str :test #'eql) 1))
         (x-start (+ id-end 3))
         (x-end (position #\, str :test #'eql))
         (y-start (+ x-end 1))
         (y-end (position #\: str :test #'eql))
         (width-start (+ y-end 2))
         (width-end (position #\x str :test #'eql))
         (height-start (+ width-end 1))
         (height-end (length str))
         (id (read-from-string (subseq str id-start id-end)))
         (x (read-from-string (subseq str x-start x-end)))
         (y (read-from-string (subseq str y-start y-end)))
         (width (read-from-string (subseq str width-start width-end)))
         (height (read-from-string (subseq str height-start height-end))))
    (make-claim :id id :x x :y y :width width :height height)))

(defun coordinates-of-claim (claim)
  "Creates string representation of claim coordinates
   to use as the key in the hash table."
  (flatten
    (let* ((x-start (+ (claim-x claim) 1))
           (x-end (+ x-start (claim-width claim) -1))
           (y-start (+ (claim-y claim) 1))
           (y-end (+ y-start (claim-height claim) -1)))
      (loop for x from x-start to x-end
            collect (loop for y from y-start to y-end
                          collect (concatenate 'string
                                               (write-to-string x)
                                               "."
                                               (write-to-string y)))))))

(defun make-fabric (claims)
  "Hash map stores list of ids of claims for every
  claimed square inch of fabric."
  (let ((fabric (make-hash-table :test #'equal)))
    (mapcar (lambda (claim)
              (mapcar (lambda (coordinates)
                        (setf (gethash coordinates fabric)
                              (cons (claim-id claim)
                                    (gethash coordinates fabric))))
                      (coordinates-of-claim claim)))
            claims)
    fabric))

(defun count-overlapping-inches (claims)
  "Drop all coordinates that have only single claim
   attached to them, and returns the number of square
   inches claimed by more than one claim."
  (let ((fabric (make-fabric claims)))
    (maphash (lambda (k v)
               (if (= (length v) 1)
                   (remhash k fabric)))
             fabric)
    (hash-table-count fabric)))

(defun find-non-overlapping-claim (claims)
  "Remembers all claims with overlaps, selects one
   without any."
  (let ((fabric (make-fabric claims))
        (overlapping-ids nil))
    (maphash (lambda (k v)
               (if (> (length v) 1)
                   (setf overlapping-ids
                         (union overlapping-ids v))))
             fabric)
    (maphash (lambda (k v)
               (mapcar (lambda (id)
                         (if (not (member id overlapping-ids))
                             (return-from find-non-overlapping-claim id)))
                       v))
             fabric)))

(defun day3 ()
  (let* ((input (file-to-list "day3.input"))
         (claims (map 'list #'parse-claim-string input))
         (overlapping-inches (count-overlapping-inches claims))
         (non-overlapping-claim (find-non-overlapping-claim claims)))
    (format t "~a claims have been collected~%" (length claims))
    (format t "~a inches have overlaps~%" overlapping-inches)
    (format t "Non-overlapping claim id: ~a" non-overlapping-claim)))

