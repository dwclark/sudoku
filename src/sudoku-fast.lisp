(defpackage :sudoku-fast
  (:use :cl)
  (:import-from :alexandria :map-product :define-constant :curry :copy-array)
  (:import-from :array-operations :each-index! :flatten)
  (:export :*peers* :*units* :at :from-standard :to-standard))

(in-package :sudoku-fast)

(define-constant +grid-size+ 81 :test #'eq)
(define-constant +units-size+ 27 :test #'eq)
(define-constant +peers-size+ 20 :test #'eq)

(deftype grid ()
  `(simple-array fixnum (,+grid-size+)))

(deftype grid-sub-array (len)
  `(simple-array fixnum (,len)))

(deftype peers-array ()
  `(simple-array fixnum (,+peers-size+)))

(deftype grid-of-peers ()
  `(simple-array peers-array (,+grid-size+)))

(deftype units-array ()
  `(simple-array fixnum (,+units-size+)))

(deftype grid-of-units ()
  `(simple-array units-array (,+grid-size+)))

(define-constant *standard-rows* "ABCDEFGHI" :test #'equal)
(define-constant *rows* '(0 1 2 3 4 5 6 7 8) :test #'equal)
(define-constant *cols* '(0 1 2 3 4 5 6 7 8) :test #'equal)
(define-constant *initial* '(1 2 3 4 5 6 7 8 9) :test #'equal)
(define-constant *coordinates* (map-product #'cons *rows* *cols*) :test #'equal)
(define-constant +starting+ #b111111111 :test #'eq)
(define-constant +bad-grid+ (make-array +grid-size+ :element-type 'fixnum :initial-element 0) :test #'equalp)

(declaim (inline square-length square-empty-p square-member square-remove square-val at put-at units-at peers-at))

(declaim (ftype (function (fixnum) fixnum) square-length))
(defun square-length (val)
  (declare (optimize speed (safety 0)))
  (logcount val))

(declaim (ftype (function (fixnum) boolean) square-empty-p))
(defun square-empty-p (val)
  (declare (optimize speed (safety 0)))
  (zerop val))

(declaim (ftype (function (fixnum fixnum) boolean) square-member))
(defun square-member (test-for val)
  (declare (optimize speed (safety 0)))
  (logbitp (1- test-for) val))

(declaim (ftype (function ((integer 1 9) fixnum) fixnum) square-remove))
(defun square-remove (to-remove val)
  (declare (optimize speed (safety 0)))
  (logand val (lognot (ash 1 (1- to-remove)))))

(declaim (ftype (function (fixnum) fixnum) square-val))
(defun square-val (val)
  (declare (optimize speed (safety 0)))
  (dotimes (n 9)
    (if (/= 0 (logand val (ash 1 n)))
        (return-from square-val (1+ n))))
  0) ;;prevent compiler warning

(declaim (ftype (function (fixnum) list) square-vals))
(defun square-vals (val)
  (declare (optimize speed))
  (let ((ret nil))
    (dotimes (n 9)
      (if (/= 0 (logand val (ash 1 n)))
          (push (1+ n) ret)))
    (nreverse ret)))
  
(defun new-grid ()
  (make-array 81 :element-type 'fixnum :initial-element +starting+))

(defun copy-grid (grid)
  (copy-array grid))

(declaim (ftype (function (grid fixnum) fixnum) at))
(defun at (grid coord)
  (declare (optimize speed (safety 0)))
  (aref grid coord))

(declaim (ftype (function (grid fixnum fixnum) *) put-at))
(defun put-at (grid coord val)
  (declare (optimize speed (safety 0)))
  (setf (aref grid coord) val))

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

(defun populate-sub-array (index all-units)
  (loop with look-for = (cons (floor (/ index 9)) (mod index 9))
        with i = -1
        with sub-array = (make-array 27 :element-type 'fixnum)
        for cells in all-units
        do (if (member look-for cells :test #'equal)
               (loop for cell in cells
                     do (setf (aref sub-array (incf i)) (+ (* 9 (car cell)) (cdr cell)))))
        finally (return sub-array)))

(defun units-grid ()
  (loop with ret = (make-array 81 :element-type `(grid-sub-array 27))
        with all-units = (append (col-units) (row-units) (box-units))
        for index from 0 below 81
        do (setf (aref ret index) (populate-sub-array index all-units))
        finally (return ret)))
                                          
(defparameter *units* (units-grid))

(declaim (ftype (function (fixnum) units-array) units-at))
(defun units-at (coord)
  (declare (optimize speed (safety 0))
           (grid-of-units *units*))
  (aref *units* coord))

(defun peers-grid (grid)
  (loop with ret = (make-array 81 :element-type `(grid-sub-array 20))
        for sub across grid and index from 0
        do (setf (aref ret index) (remove index (remove-duplicates sub)))
        finally (return ret)))

(defparameter *peers* (peers-grid *units*))

(declaim (ftype (function (fixnum) peers-array) peers-at))
(defun peers-at (coord)
  (declare (optimize speed (safety 0))
           (grid-of-peers *peers*))
  (aref *peers* coord))

(declaim (ftype (function (grid fixnum fixnum) grid) assign))
(defun assign (grid coord val)
  (declare (optimize speed (safety 0)))
  (loop with square-values = (at grid coord)
        for other-val from 1 upto 9
        do (if (and (/= val other-val)
                    (square-member other-val square-values)
                    (eq +bad-grid+ (eliminate grid coord other-val)))
               (return +bad-grid+))
        finally (return grid)))

(declaim (ftype (function (grid fixnum fixnum) fixnum) val-places-in-unit))
(defun val-places-in-unit (grid coord val)
  (declare (optimize speed (safety 0))
           (grid *units*))
  (loop with ret = -1
        for unit-coord across (units-at coord)
        do (let ((vals (at grid unit-coord)))
             (if (square-member val vals)
                 (if (/= -1 val)
                     (return -2)
                     (setf ret val))))
        finally (return ret)))

(declaim (ftype (function (grid fixnum fixnum) *) eliminate))
(defun eliminate (grid coord val)
  (declare (optimize speed (safety 0))
           (grid *peers* *units*))
  (if (not (square-member val (at grid coord)))
      (return-from eliminate grid))
  
  (put-at grid coord (square-remove val (at grid coord)))
  
  (if (square-empty-p (at grid coord))
      (return-from eliminate +bad-grid+))
  
  (if (= 1 (square-length (at grid coord)))
      (loop with other-val = (square-val (at grid coord))
            for peer across (peers-at coord)
            do (if (eq +bad-grid+ (eliminate grid peer other-val))
                   (return-from eliminate +bad-grid+))))
  
  (let ((remaining (val-places-in-unit grid coord val)))
    (if (= -1 remaining)
        (return-from eliminate +bad-grid+))
    
    (if (and (<= 0 remaining)
             (eq +bad-grid+ (assign grid remaining val)))
        (return-from eliminate +bad-grid+)))
  
  grid)
        
(defun parse-single-line-grid (str)
  (let ((ret (new-grid)))
    (loop for c across str and index from 0
          do (let ((digit (digit-char-p c)))
               (if (and (not (null digit))
                        (/= 0 digit))
                   (if (eq +bad-grid+ (assign ret index digit))
                       (return +bad-grid+))))
          finally (return ret))))

(declaim (ftype (function (grid) boolean) solved-p))
(defun solved-p (grid)
  (declare (optimize speed (safety 0)))
  (loop for val across grid
        do (if (/= 1 (square-length val))
               (return nil))
        finally (return t)))

(declaim (ftype (function (grid) fixnum) easiest))
(defun easiest (grid)
  (declare (optimize speed))
  (loop with min-so-far = 9
        with ret = -1
        for val across grid and index fixnum from 0
        do (let ((new-len (square-length val)))
             (if (and (/= 1 new-len)
                      (< new-len min-so-far))
                 (setf min-so-far new-len
                       ret index))
             (if (= 2 new-len) (return ret)))
        finally (progn
                  (if (zerop (square-length (at grid ret)))
                      (break))
                  (return ret))))

(defun find-solution (grid)
  (if (or (eq +bad-grid+ grid)
          (solved-p grid))
      (return-from find-solution grid))
  
  (let ((coord (easiest grid)))
    (loop for d from 1 upto 9
          do (if (square-member d (at grid coord)) 
                 (let ((result (find-solution (assign (copy-grid grid) coord d))))
                   (if (not (eq +bad-grid+ result))
                       (return result))))
          finally (return +bad-grid+))))

(defun solve (str)
  (find-solution (parse-single-line-grid str)))

(defun read-puzzles (file-name)
  (with-open-file (stm file-name)
    (loop for line = (read-line stm nil)
          while line
          collect line)))

(defun solve-puzzles (lst)
  (dolist (str lst)
    (let ((solution (solve str)))
      (if (not (eq +bad-grid+ solution))
          (format t "~&solved-puzzle")
          (format t "~&no solution to puzzle")))))
  
(defun display (grid)
  (let ((ret (make-array '(9 9) :element-type 'cons)))
    (loop for index below (length grid)
          do (setf (aref ret (floor (/ index 9)) (mod index 9))
                   (square-vals (aref grid index)))
          finally (return ret))))
