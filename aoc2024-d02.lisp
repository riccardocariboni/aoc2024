;;;; aoc2024-d02.lisp

(in-package #:aoc2024-d02)

(defun parse-input (file)
  (let ((lines (uiop:read-file-lines file))
	(out nil))
    (dolist (line lines)
      (let ((l nil))
	(dolist (n (uiop:split-string line))
	  (push (parse-integer n) l))
	(push (reverse l) out)))
    (reverse out)))

(defun check-safe (l)
  (let ((ascending (if (< (first l) (second l))
		       t
		       nil)))
    (loop :for i :from 0 :to (- (length l) 2)
	  :do
	     (let ((diff (abs (- (nth (1+ i) l) (nth i l)))))
	       (if ascending
		   (unless (and (> (nth (1+ i) l) (nth i l))
				(>= diff 1)
				(<= diff 3))
		     (return-from check-safe nil))
		   (unless (and (< (nth (1+ i) l) (nth i l))
				(>= diff 1)
				(<= diff 3))
		     (return-from check-safe nil)))))
    t))

(defun solve-p1 (file)
  (let ((reports (parse-input file))
	(safe 0))
    (dolist (r reports)
      (if (check-safe r)
	  (setf safe (1+ safe))))
    safe))

(defun check-safe-2 (l)
  (if (check-safe l)
      t
      (progn
	(dotimes (r (length l))
	  (when (check-safe (concatenate 'list
					 (subseq l 0 r)
					 (subseq l (1+ r) (length l))))
	    (return-from check-safe-2 t)))
	nil)))

(defun solve-p2 (file)
  (let ((reports (parse-input file))
	(safe 0))
    (dolist (r reports)
      (if (check-safe-2 r)
	  (setf safe (1+ safe))))
    safe))
