(load "utils.lisp")

(defun react-p (x y)
  "Answers if two units react with each other."
  (and (not (= (char-code x) (char-code y)))
       (char-equal x y)))

(defun remove-pair (str i)
  "Deletes a couple of chars from the string at the specified index."
  (concatenate 'string (subseq str 0 i) (subseq str (+ i 2))))

(defun react-polymer (str)
  "Fully react given polymer."
  (do ((i 0))
      ((>= i (- (length str) 1)) str) ; end condition and return form
      (let ((curr (char str i))
            (next (char str (+ i 1))))
        (if (react-p curr next)
            (progn (setf str (remove-pair str i))
                   (setf i (max 0 (- i 1)))) ; carefull not to get negative index
            (incf i)))))

(defun construct-unit-database (polymer)
  "Finds all types of units in a polymer disregarding polarity."
  (let ((db (make-hash-table)))
    (loop for unit across polymer
          do (setf (gethash (char-downcase unit) db) t))
    (hash-table-keys db)))

(defun remove-unit (unit polymer)
  "Completely removes specified unit from the polymer
   in both polarities."
  (remove (char-upcase unit) (remove unit polymer)))

(defun find-optimized-polymer (polymer unit-database)
  "Finds optimal unit to remove."
  (let ((min-length (length polymer))
        (best-removal-candidate nil)
        (shortest-polymer nil))
    (mapcar (lambda (unit)
              (let* ((polymer-after-removal (react-polymer (remove-unit unit polymer)))
                     (length-after-removal (length polymer-after-removal)))
                (if (< length-after-removal min-length)
                    (progn (setf min-length length-after-removal)
                           (setf best-removal-candidate unit)
                           (setf shortest-polymer polymer-after-removal)))))
            unit-database)
    ; (format t "Best removal candidate: ~a~%" best-removal-candidate)
    shortest-polymer))

(defun day5 ()
  (let* ((input (car (file-to-list "day5.input")))
         (reacted (react-polymer input))
         (unit-database (construct-unit-database input))
         (optimized-polymer (find-optimized-polymer input unit-database)))
    (format t "Unreacted polymer has ~a units~%" (length input))
    (format t "Reacted polymer has ~a units~%" (length reacted))
    (format t "Source polymer has ~a unit types~%" (length unit-database))
    (format t "Optimized polymer has ~a units" (length optimized-polymer))))

