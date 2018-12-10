(load "utils.lisp")

(defstruct guard
  asleep minutes)

(defun parse-guard-id (str)
  (read-from-string (subseq (nth 3 (split str)) 1)))

(defun parse-minute (str)
  (read-from-string (subseq str 15 17)))

(defun guard-string-p (str)
  (equal (nth 2 (split str)) "Guard"))

(defun separate-input-by-guard (input)
  "Converts multiple lines of guard data into single
   list of minutes."
  (let ((guards (make-hash-table))
        (current-guard-id nil))
    (loop for str in input
          do (if (guard-string-p str)
                 (setf current-guard-id (parse-guard-id str))
                 (setf (gethash current-guard-id guards)
                       (append (gethash current-guard-id guards)
                               (list (parse-minute str))))))
    guards))

(defun unroll-time-intervals (times)
  "Expands pair of numbers representing time interval into full
   list of minutes.
   Example: 1 3 5 8 => 1 2 5 6 7"
  (let ((result))
    (loop for x on times by #'cddr
          do (let ((from (car x))
                   (to (cadr x)))
               (setf result
                     (append result (range to :min from)))))
    result))

(defun unroll-guards-times (guards)
  "Applies unrolling to every guard."
  (let ((unrolled-guards (make-hash-table)))
    (maphash (lambda (k v)
               (setf (gethash k unrolled-guards)
                     (unroll-time-intervals v)))
             guards)
    unrolled-guards))

(defun find-sleepiest-guard-1 (unrolled-guards)
  "Find the guard that has the most minutes asleep."
  (let ((sleepiest-guard nil)
        (max-minutes 0))
    (maphash (lambda (guard minutes)
               (if (> (length minutes) max-minutes)
                   (progn (setf max-minutes (length minutes))
                          (setf sleepiest-guard guard))))
             unrolled-guards)
    sleepiest-guard))

(defun find-sleepiest-minute-1 (minutes)
  "Finds on which minute guard sleeps the most."
  (let ((sleepiest-minute nil)
        (max-times 0))
    (loop for minute in minutes
          do (if (> (count minute minutes) max-times)
                 (progn (setf max-times (count minute minutes))
                        (setf sleepiest-minute minute))))
    sleepiest-minute))

(defun compress-guards-times (unrolled-guards)
  "For each guard constructs a hash table with minutes as keys
   and number of times guard was asleep on that minute as values."
  (let ((compressed-guards (make-hash-table)))
    (maphash (lambda (guard unrolled-minutes)
               (let ((compressed-minutes (make-hash-table)))
                 (loop for minute in unrolled-minutes
                       do (if (not (gethash minute compressed-minutes))
                              (setf (gethash minute compressed-minutes)
                                    (count minute unrolled-minutes))))
                 (setf (gethash guard compressed-guards)
                       compressed-minutes)))
             unrolled-guards)
    compressed-guards))

(defun find-sleepiest-guard-2 (compressed-guards)
  "Finds which guard is most frequently asleep on the same minute."
  (let ((sleepiest-guard nil)
        (sleepiest-minute nil)
        (max-times 0))
    (maphash (lambda (guard minutes)
               (maphash (lambda (minute times)
                          (if (> times max-times)
                              (progn (setf max-times times)
                                     (setf sleepiest-minute minute)
                                     (setf sleepiest-guard guard))))
                        minutes))
             compressed-guards)
    (list sleepiest-guard sleepiest-minute)))

(defun day4 ()
  (let* ((input (sort (file-to-list "day4.input") #'string-lessp))
         (guards (separate-input-by-guard input))
         (unrolled-guards (unroll-guards-times guards))
         (sleepiest-guard-1 (find-sleepiest-guard-1 unrolled-guards))
         (sleepiest-minute-1
           (find-sleepiest-minute-1 (gethash sleepiest-guard-1 unrolled-guards)))
         (compressed-guards (compress-guards-times unrolled-guards))
         ;; I couldn't find how to use multiple-value-bind with the let macro,
         ;; so I had to return values as list and just called the function twice.
         (sleepiest-guard-2 (car (find-sleepiest-guard-2 compressed-guards)))
         (sleepiest-minute-2 (cadr (find-sleepiest-guard-2 compressed-guards))))
    (format t "~a guards are found~%" (hash-table-count guards))
    (format t "    Strategy 1:~%")
    (format t "The sleepiest guard ~a slept the most on the ~a minute (~a)~%"
            sleepiest-guard-1
            sleepiest-minute-1
            (* sleepiest-guard-1 sleepiest-minute-1))
    (format t "    Strategy 2:~%")
    (format t "The sleepiest guard ~a slept the most on the ~a minute (~a)~%"
            sleepiest-guard-2
            sleepiest-minute-2
            (* sleepiest-guard-2 sleepiest-minute-2))))

