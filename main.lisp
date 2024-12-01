
(in-package #:com.thejach.aoc24)

;; Day 1

(defvar sample1
"
3   4
4   3
2   5
1   3
3   9
3   3
")

(defun lines (str)
  (remove "" (cl-ppcre:split "\\n" str) :test #'string-equal))

(defun transpose (m)
  (apply #'mapcar #'list m))

(defun columns (str)
  (transpose
    (loop for line in (lines str)
          collect
          (let ((cols (mapcar #'parse-integer (cl-ppcre:split "[ ]+" line))))
            cols))))

(let* ((cols (columns sample1))
       (left (sort (first cols) #'<))
       (right (sort (second cols) #'<)))
  (reduce #'+ (mapcar (lambda (l r) (abs (- r l))) left right)))


(defun day1 (&aux input)
  (setf input sample1)
  (setf input *day1-input*)
  (flet ((part1 ()
           (let* ((cols (columns input))
                  (left (sort (first cols) #'<))
                  (right (sort (second cols) #'<)))
             (reduce #'+ (mapcar (lambda (l r) (abs (- r l))) left right))))
         (part2 ()
           (let* ((cols (columns input))
                  (right-freq (make-hash-table))
                  (similarity-score 0))
             (loop for r in (second cols)
                   do
                   (incf (gethash r right-freq 0)))
             (loop for l in (first cols)
                   do
                   (incf similarity-score (* l (gethash l right-freq 0))))
             similarity-score)))
    (list (part1) (part2))))

(day1)
