;;;; aoc2024-d01.lisp

(in-package #:aoc2024-d01)

(defun parse-input (file)
  (let ((lines (uiop:read-file-lines file))
	(fl nil)
	(sl nil))
    (dolist (line lines)
      (let ((l (uiop:split-string line)))
	(push (parse-integer (first l)) fl)
	(push (parse-integer (fourth l)) sl)))
    (list fl sl)))

(defun dist (a b)
  (- (max a b) (min a b)))

(defun solve-p1 (file)
  (let ((fl (sort (first (parse-input file)) #'<))
	(sl (sort (second (parse-input file)) #'<))
	(sum 0))
    (dotimes (n (length fl))
      (setf sum (+ sum (dist (pop fl) (pop sl)))))
    sum))

(defun solve-p2 (file)
  (let ((fl (first (parse-input file)))
	(sl (second (parse-input file)))
	(occ (make-hash-table))
	(score 0))
    (dolist (i fl)
      (if (gethash i occ)
	  (setf score (+ score (gethash i occ)))
	  (progn
	    (setf (gethash i occ) (* i (count i sl)))
	    (setf score (+ score (gethash i occ))))))
    score))
