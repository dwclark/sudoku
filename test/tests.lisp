(defpackage :sudoku-tests
  (:use :cl :fiveam :sudoku))

(in-package :sudoku-tests)

(def-suite sudoku-tests :description "Sudoku Tests")
(in-suite sudoku-tests)

(test test-peers
  (is (equal (sort (mapcar #'to-standard (at *peers* (from-standard "C2"))) #'string<)
             (sort (list "A2" "B2" "D2" "E2" "F2" "G2" "H2" "I2"
                         "C1" "C3" "C4" "C5" "C6" "C7" "C8" "C9"
                         "A1" "A3" "B1" "B3") #'string<))))

(test test-units
  (is (equal (sort (mapcar #'to-standard (at *units* (from-standard "C2"))) #'string<)
             (sort (list "A2" "B2" "C2" "D2" "E2" "F2" "G2" "H2" "I2"
                         "C1" "C2" "C3" "C4" "C5" "C6" "C7" "C8" "C9"
                         "A1" "A2" "A3" "B1" "B2" "B3" "C1" "C2" "C3") #'string<))))
