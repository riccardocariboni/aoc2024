;;;; aoc2024-d05.lisp

(in-package #:aoc2024-d05)

(defun parse-input (file)
  (let ((in (uiop:read-file-lines file))
	(rl nil))
    (loop :while (not (string= "" (first in)))
	  :do
	  (push (pop in) rl))
    (list (reverse rl) (rest in))))

(defun sort-u (u rules)
  (sort (copy-list u)
	(lambda (a b)
	  (member (list a b) rules :test #'equal))))

(defun check-update (u rules)
  (when (loop :for (a b) :in rules
	      :if (and (member a u)
		       (member b u))
		:unless (< (position a u) (position b u))
		  :return nil
	      :finally (return t))
    (elt u (truncate (length u) 2))))

(defun solve-p1-and-p2 (file)
  (let ((rules-list (first (parse-input file)))
	(updates-list (second (parse-input file)))
	(r nil)
	(u nil))
    (setf r (mapcar #'(lambda (x)
			(mapcar #'parse-integer (uiop:split-string x :separator '(#\|))))
		    rules-list))
    (setf u (mapcar #'(lambda (x)
			(mapcar #'parse-integer (uiop:split-string x :separator '(#\,))))
		    updates-list))

    (loop :for up :in u
	  :when (check-update up r)
	    :sum :it :into s1
	  :else :sum (elt (sort-u up r)
			  (truncate (length up) 2))
	  :into s2
	  :finally (return (values s1 s2)))))
