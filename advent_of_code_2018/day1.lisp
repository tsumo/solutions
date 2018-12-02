(load "utils.lisp")

;;; Enable circular lists
(setf *print-circle* t)

(defun circular (lst)
  "Creates circular never-ending lists."
  (setf (cdr (last lst)) lst))

(defun find-repeated-value (lst)
  "Continues to add together numbers from the generator lst, waits until some
   resulting value repeat twice."
  (do ((current-value 0)
       (seen-values nil))
      ((member current-value seen-values) current-value)
      (progn (setf seen-values (cons current-value seen-values))
             (setf current-value (+ current-value (pop lst))))))

(defun day1 ()
  (let* ((input (map 'list #'read-from-string (file-to-list "day1.input")))
         (resulting-frequency (reduce #'+ input :initial-value 0))
         (circular-input (circular input))
         (repeated-value (find-repeated-value circular-input)))
    (format t "  *** Error: Device must be calibrated before first use.~%")
    (format t "  Frequency drift detected. Cannot maintain destination lock. ***~%")
    (format t "Calculated resulting frequency: ~a~%" resulting-frequency)
    (format t "Starting calibration procedure~%")
    (format t "Calculated repeated value: ~a~%" repeated-value)
    (format t "Device has been reconfigured successfully")))

