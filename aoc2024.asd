;;;; aoc2024.asd

(asdf:defsystem #:aoc2024
  :description "Advent of Code 2024"
  :author "Riccardo Cariboni <info@riccardocariboni.it>"
  :license  "©2024 Riccardo Cariboni"
  :version "0.0.1"
  :depends-on ("cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "aoc2024-d01")
	       (:file "aoc2024-d02")
	       (:file "aoc2024-d03")
	       (:file "aoc2024-d04")))
