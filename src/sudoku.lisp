(defpackage :sudoku
  (:use :cl)
  (:import-from :alexandria :map-product :define-constant :curry :copy-array)
  (:import-from :array-operations :each-index! :flatten)
  (:export :*peers* :*units* :at :from-standard :to-standard))

(in-package :sudoku)

(define-constant *standard-rows* "ABCDEFGHI" :test #'equal)
(define-constant *rows* '(0 1 2 3 4 5 6 7 8) :test #'equal)
(define-constant *cols* '(0 1 2 3 4 5 6 7 8) :test #'equal)
(define-constant *initial* '(1 2 3 4 5 6 7 8 9) :test #'equal)
(define-constant *coordinates* (map-product #'cons *rows* *cols*) :test #'equal)

(defun new-grid ()
  (make-array '(9 9) :element-type 'cons :initial-element *initial*))

(defun copy-grid (grid)
  (copy-array grid))

(defun at (grid coord)
  (aref grid (car coord) (cdr coord)))

(defun at-alist (grid coords)
  (mapcar #'(lambda (coord)
              (cons coord (at grid coord))) coords))

(defun put-at (grid coord val)
  (setf (aref grid (car coord) (cdr coord)) val))

(defun col-units ()
  (mapcar #'(lambda (c) (map-product #'cons *rows* (list c))) *cols*))

(defun row-units ()
  (mapcar #'(lambda (r) (map-product #'cons (list r) *cols*)) *rows*))

(defun box-units ()
  (loop with boxes = nil
        for row in '((0 1 2) (3 4 5) (6 7 8))
        do (loop for col in '((0 1 2) (3 4 5) (6 7 8))
                 do (push (map-product #'cons row col) boxes))
        finally (return (nreverse boxes))))

(defun units-grid ()
  (let ((ret (make-array '(9 9) :element-type 'cons))
        (all-units (append (col-units) (row-units) (box-units))))
    (each-index! ret (r c)
      (loop with cell = (cons r c)
            for cells in all-units
            appending (if (member cell cells :test #'equal) cells nil) into units
            finally (return units)))))

(define-constant *units* (units-grid) :test #'equalp)

(defun peers-grid (grid)
  (let ((ret (make-array '(9 9) :element-type 'cons)))
    (each-index! ret (r c)
      (let ((cell (cons r c)))
        (remove cell (remove-duplicates (at grid (cons r c)) :test #'equal) :test #'equal)))))

(define-constant *peers* (peers-grid *units*) :test #'equalp)

(defun to-standard (cell)
  (format nil "~A~A" (char *standard-rows* (car cell)) (1+ (cdr cell))))

(defun from-standard (str)
  (cons (position (char str 0) *standard-rows*)
        (1- (parse-integer (subseq str 1)))))

(defun assign (grid coord val)
  (loop with to-eliminate = (remove val (at grid coord))
        for other-val in to-eliminate
        do (if (not (eliminate grid coord other-val)) (return nil))
        finally (return grid)))

(defun eliminate (grid coord val)
  (if (not (member val (at grid coord)))
      (return-from eliminate grid))

  (put-at grid coord (remove val (at grid coord)))
  
  (if (zerop (length (at grid coord)))
      (return-from eliminate nil))

  (if (= 1 (length (at grid coord)))
      (loop with other-val = (first (at grid coord))
            for peer in (at *peers* coord)
            do (if (not (eliminate grid peer other-val)) (return-from eliminate nil))))

  (loop with alist = (at-alist grid (at *units* coord))
        for d in *initial*
        do (let ((d-places (remove-if-not (curry #'member d) alist :key #'cdr)))
             (if (zerop (length d-places))
                 (return-from eliminate nil))

             (if (and (= 1 (length d-places))
                      (not (assign grid (caar d-places) d)))
                 (return-from eliminate nil))))
  grid)

(defun parse-single-line-grid (str)
  (let ((ret (new-grid)))
    (loop for c across str and index from 0
          do (let ((digit (digit-char-p c)))
               (if (and (not (null digit))
                        (/= 0 digit))
                   (if (null (assign ret (cons (floor (/ index 9)) (mod index 9)) digit))
                       (return nil))))
          finally (return ret))))

(defun solved-p (grid)
  (every (curry #'= 1) (map 'vector #'length (flatten grid))))

(defun easiest (grid)
  (loop with len = 9
        with ret = nil
        for coord in *coordinates*
        do (let ((vals (at grid coord)))
             (if (and (/= 1 (length vals))
                      (< (length vals) len))
                 (setf len (length vals)
                       ret coord)))
        finally (return ret)))

(defun find-solution (grid)
  (if (or (null grid)
          (solved-p grid)) (return-from find-solution grid))

  (let ((coord (easiest grid)))
    (loop for d in (at grid coord)
          do (let ((result (find-solution (assign (copy-grid grid) coord d))))
               (if result (return result)))
          finally (return nil))))

(defun solve (str)
  (find-solution (parse-single-line-grid str)))

(defun read-puzzles (file-name)
  (with-open-file (stm file-name)
    (loop for line = (read-line stm nil)
          while line
          collect line)))

(defun solve-puzzles (lst)
  (dolist (str lst)
    (if (solve str)
        (format t "~&solved puzzle")
        (format t "~& no solution to puzzle"))))
