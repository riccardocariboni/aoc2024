;;;; aoc2024-d06.lisp

(in-package #:aoc2024-d06)

(defun parse-input (file)
  (let ((in (uiop:read-file-lines file)))
    in))

(define-condition guard-loop ()
  ((steps
    :initarg :steps
    :accessor guard-loop-steps)))

(defun parse-map (lines)
  (make-array (list (length (car lines))
                    (length lines))
              :element-type 'character
              :initial-contents lines))

(defun copy-map (array)
  (let ((dimensions (array-dimensions array)))
    (adjust-array (make-array dimensions
                              :element-type (array-element-type array)
                              :adjustable t :displaced-to array)
                  dimensions)))

(defun display-map (map &optional (stream *standard-output*))
  (dotimes (y (array-dimension map 0))
    (dotimes (x (array-dimension map 1))
      (write-char (aref map y x) stream))
    (terpri)))

(defun tile (pos map)
  (aref map (cdr pos) (car pos)))

(defun (setf tile) (value pos map)
  (setf (aref map (cdr pos) (car pos)) value))

(defmacro dotiles ((pos map &optional result) &body body)
  (let ((map-var (gensym "MAP-"))
        (x (gensym "X-"))
        (y (gensym "Y-")))
    `(let ((,map-var ,map))
       (dotimes (,y (array-dimension ,map-var 0))
         (dotimes (,x (array-dimension ,map-var 1))
           (let ((,pos (cons ,x ,y)))
             ,@body)))
       ,result)))

(defun count-visited-tiles (map)
  (let ((count 0))
    (dotiles (pos map count)
      (when (char= (tile pos map) #\^)
        (incf count)))))

(defun initial-guard-pos (map)
  (dotiles (pos map)
    (when (char= (tile pos map) #\^)
      (return-from initial-guard-pos pos))))

(defun move-guard (map)
  (do ((pos (initial-guard-pos map))
       (direction :up)
       (steps nil)
       (step-table (make-hash-table :test #'equal)))
      (nil)
    (let ((next-pos (next-pos pos direction)))
      (unless (valid-pos next-pos map)
        (return-from move-guard nil))
      (ecase (tile next-pos map)
        ((#\. #\^)
         (setf (tile next-pos map) #\^)
         (let ((step (cons pos direction)))
           (when (gethash step step-table)
             (signal 'guard-loop :steps steps))
           (setf (gethash step step-table) t)
           (push step steps))
         (setf pos next-pos))
        ((#\# #\O)
         (setf direction (turn-right direction)))))))

(defun valid-pos (pos map)
  (and (<= 0 (car pos) (1- (array-dimension map 1)))
       (<= 0 (cdr pos) (1- (array-dimension map 0)))))

(defun next-pos (pos direction)
  (let ((x (car pos))
        (y (cdr pos)))
    (ecase direction
      (:up
       (cons x (1- y)))
      (:right
       (cons (1+ x) y))
      (:down
       (cons x (1+ y)))
      (:left
       (cons (1- x) y)))))

(defun turn-right (direction)
  (ecase direction
    (:up
     :right)
    (:right
     :down)
    (:down
     :left)
    (:left
     :up)))

(defun solve-p1 (file)
  (let ((map (parse-map (parse-input file))))
    (move-guard map)
    (count-visited-tiles map)))

(defun solve-p2 (file)
  (let* ((map (parse-map (parse-input file)))
         (ref-map (copy-map map))
         (initial-pos (initial-guard-pos map))
         (loops nil))
    (move-guard ref-map)
    (dotiles (pos ref-map)
      (when (and (char= (tile pos ref-map) #\^)
                 (not (equal pos initial-pos)))
        (let ((map2 (copy-map map)))
          (setf (tile pos map2) #\O)
          (handler-case
              (move-guard map2)
            (guard-loop (condition)
              (push (guard-loop-steps condition) loops))))))
    (length (remove-duplicates loops :test #'equal))))
