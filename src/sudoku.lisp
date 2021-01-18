(defpackage :sudoku
  (:use :cl)
  (:import-from :alexandria :map-product :define-constant :curry :copy-array)
  (:import-from :array-operations :each-index! :flatten)
  (:export :*peers* :*units* :at :from-standard :to-standard))

(in-package :sudoku)

(deftype grid ()
  `(simple-array list (9 9)))

(define-constant *standard-rows* "ABCDEFGHI" :test #'equal)
(define-constant *rows* '(0 1 2 3 4 5 6 7 8) :test #'equal)
(define-constant *cols* '(0 1 2 3 4 5 6 7 8) :test #'equal)
(define-constant *initial* '(1 2 3 4 5 6 7 8 9) :test #'equal)
(define-constant *coordinates* (map-product #'cons *rows* *cols*) :test #'equal)

(defun new-grid ()
  (make-array '(9 9) :element-type 'list :initial-element *initial*))

(defun copy-grid (grid)
  (copy-array grid))

(declaim (inline at))
(declaim (ftype (function (grid cons) *) at))
(defun at (grid coord)
  (declare (optimize speed))
  (let ((row (car coord))
        (col (cdr coord)))
    (declare (fixnum row col))
  (aref grid row col)))

(declaim (inline put-at))
(declaim (ftype (function (grid cons list) *) put-at))
(defun put-at (grid coord val)
  (declare (optimize (debug 3)))
  (let ((row (car coord))
        (col (cdr coord)))
    (declare (fixnum row col))
  (setf (aref grid row col) val)))

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

(defparameter *units* (units-grid))

(defun peers-grid (grid)
  (let ((ret (make-array '(9 9) :element-type 'cons)))
    (each-index! ret (r c)
      (let ((cell (cons r c)))
        (remove cell (remove-duplicates (at grid (cons r c)) :test #'equal) :test #'equal)))))

(defparameter *peers* (peers-grid *units*))

(defun to-standard (cell)
  (format nil "~A~A" (char *standard-rows* (car cell)) (1+ (cdr cell))))

(defun from-standard (str)
  (cons (position (char str 0) *standard-rows*)
        (1- (parse-integer (subseq str 1)))))

(declaim (ftype (function (grid cons fixnum) *) assign))
(defun assign (grid coord val)
  (declare (optimize speed))
  (loop for other-val fixnum in (at grid coord)
        do (if (and (/= val other-val) ; don't call remove, very expensive
                    (not (eliminate grid coord other-val)))
               (return nil))
        finally (return grid)))

(declaim (inline member-p))
(declaim (ftype (function (fixnum list) boolean) member-p))
(defun member-p (val sorted-list)
  (declare (optimize speed))
  (loop for mem fixnum in sorted-list
        do (cond ((= mem val) (return t))
                 ((< val mem) (return nil)))
        finally (return nil)))
  
(declaim (ftype (function (grid cons fixnum) *) eliminate))
(defun eliminate (grid coord val)
  (declare (optimize speed)
           (grid *peers* *units*))
  (if (not (member-p val (at grid coord)))
      (return-from eliminate grid))

  (put-at grid coord (remove val (at grid coord) :test 'eq))
  
  (if (null (at grid coord)) ;fast test length = 0
      (return-from eliminate nil))

  (if (null (cdr (at grid coord))) ; fast test length = 1
      (loop with other-val = (first (at grid coord))
            for peer in (at *peers* coord)
            do (if (not (eliminate grid peer other-val)) (return-from eliminate nil))))

  (loop for other-coord in (at *units* coord)
        do (let ((remaining (at grid other-coord)))
             (if (member-p val remaining)
                 (progn
                   (if (null remaining) ; fast test length = 0
                       (return-from eliminate nil))
                   
                   (if (and (null (cdr remaining)) ; fast test length = 1
                            (not (assign grid other-coord val)))
                       (return-from eliminate nil))))))
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
  (every (curry #'= 1) (map 'vector #'list-length (flatten grid))))

(defun easiest (grid)
  (declare (optimize speed))
  (loop with len = 9
        with ret = nil
        for coord in *coordinates*
        do (let* ((vals (at grid coord))
                  (new-len (list-length vals)))
             (if (and (/= 1 new-len)
                      (< new-len len))
                 (setf len new-len
                       ret coord))
             (if (= 2 len) (return ret)))
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
