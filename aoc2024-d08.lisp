;;;; aoc2024-d08.lisp

(in-package #:aoc2024-d08)

(defun parse-input (file)
  (with-open-file (f file)
    (labels ((recur ()
               (let ((line (read-line f nil nil)))
                 (when line
                   (cons line (recur))))))
      (map 'vector #'identity (recur)))))

(defun read-antennae (grid)
  (let ((spots (make-hash-table :test #'equal)))
    (loop :for row :across grid
          :for y :from 0
	  :do
            (loop :for c :across row
                  :for x :from 0
		  :do
                    (unless (char= c #\.)
                      (push (cons x y) (gethash c spots)))))
    spots))

(defun all-antinodes (spots x-bound y-bound)
  (labels ((in-bounds (x y)
             (and (>= x 0)
                  (< x x-bound)
                  (>= y 0)
                  (< y y-bound)))
           (in-bound-nodes (coord-1 coord-2)
             (destructuring-bind ((x1 . y1) (x2 . y2)) (list coord-1 coord-2)
               (let* ((dx (abs (- x1 x2)))
                      (dy (abs (- y1 y2)))
                      (ax1 (- x1 dx))
                      (ay1 (if (> y1 y2) (+ y1 dy) (- y1 dy)))
                      (ax2 (+ x2 dx))
                      (ay2 (if (> y2 y1) (+ y2 dy) (- y2 dy))))
                 (remove nil
                         (list (when (in-bounds ax1 ay1) (cons ax1 ay1))
                               (when (in-bounds ax2 ay2) (cons ax2 ay2)))))))
           (recur (acc xs)
             (cond
               ((null xs) acc)
               ((= (length xs) 1) acc)
               (t (recur (append acc
                                 (loop :for y :in (cdr xs)
                                       :append (apply #'in-bound-nodes
                                                     (sort (list (car xs) y)
                                                           #'<
                                                           :key #'car))))
                         (cdr xs))))))
    (recur nil spots)))

(defun solve-p1 (file)
  (let* ((grid (parse-input file))
         (antennae (read-antennae grid))
         (bound-x (length (aref grid 0)))
         (bound-y (length grid)))
    (length
     (remove-duplicates
      (mapcan
       (lambda (key)
         (all-antinodes (gethash key antennae) bound-x bound-y))
       (loop :for k :being :the :hash-keys :of antennae
	     :collect k))
      :test #'equal))))

(defun all-antinodes-advanced (spots x-bound y-bound)
  (labels ((in-bounds (x y)
             (and (>= x 0)
                  (< x x-bound)
                  (>= y 0)
                  (< y y-bound)))
           (iter-in-bounds-nodes (x-in y-in dx dy)
             (append
              (loop :with x = x-in
		    :with y = y-in
                    :while (in-bounds x y)
                    :collect (cons x y)
                    :do (incf x dx)
                       (incf y dy))
              (loop :with x = x-in
		    :with y = y-in
                    :while (in-bounds x y)
                    :collect (cons x y)
                    :do (decf x dx)
                       (decf y dy))))
           (in-bound-nodes (coord-1 coord-2)
             (destructuring-bind ((x1 . y1) (x2 . y2)) (list coord-1 coord-2)
               (let ((dx (abs (- x1 x2)))
                     (dy (abs (- y1 y2))))
                 (iter-in-bounds-nodes x1 y1 dx (if (> y1 y2) (- dy) dy)))))
           (recur (acc xs)
             (cond
               ((null xs) acc)
               ((= (length xs) 1) acc)
               (t (recur (append acc
                                 (loop :for y :in (cdr xs)
                                       :append (apply #'in-bound-nodes
                                                     (sort (list (car xs) y)
                                                           #'<
                                                           :key #'car))))
                         (cdr xs))))))
    (recur nil spots)))

(defun solve-p2 (file)
  (let* ((grid (parse-input file))
         (antennae (read-antennae grid))
         (bound-x (length (aref grid 0)))
         (bound-y (length grid)))
    (length
     (remove-duplicates
      (mapcan
       (lambda (key)
         (all-antinodes-advanced (gethash key antennae) bound-x bound-y))
       (loop :for k :being :the :hash-keys :of antennae
	     :collect k))
      :test #'equal))))
