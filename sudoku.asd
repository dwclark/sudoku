;; Sudoku Game

(asdf:defsystem #:sudoku
  :description "Sudoku"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :depends-on ("alexandria" "array-operations")
  :components ((:file "src/sudoku")))

(asdf:defsystem #:sudoku-tests
  :description "Sudoku Tests"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :serial t

  :depends-on ("sudoku" "fiveam")
  :components ((:file "test/tests")))

