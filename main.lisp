
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

;; Day 2

(defvar sample2 "
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defun rows (str)
  (loop for line in (lines str)
        collect (mapcar #'parse-integer (cl-ppcre:split "[ ]+" line))))

(defun level-direction (level1 level2)
  (cond
    ((< level1 level2) :increasing)
    ((> level1 level2) :decreasing)
    (t :unsafe)))

(defun safe-report? (row &aux levels-direction)
  (setf levels-direction (level-direction (first row) (second row)))
  (unless (eql :unsafe levels-direction)
    (loop for (level . rest) on row
          when rest do
          (unless (and (eql levels-direction (level-direction level (first rest)))
                       (<= 1 (abs (- level (first rest))) 3))
            (return-from safe-report? nil)))
    t))

(defun remove-nth (seq n)
  (append (subseq seq 0 n)
          (subseq seq (1+ n) (length seq))))

(defun problem-dampener-safe? (row)
  (loop for i from 0 below (length row)
        do
        (when (safe-report? (remove-nth row i))
          (return-from problem-dampener-safe? t))))

(defun day2 (&aux input)
  (setf input sample2)
  (setf input *day2-input*)
  (flet ((part1 () ; 19 mins
           (loop for report in (rows input)
                 counting (safe-report? report)))
         (part2 () ; 25 mins more, mostly remembering how to do fancy loop and subseq stuff, so 44 mins total
           (loop for report in (rows input)
                 for safe = (safe-report? report)
                 counting (or safe (problem-dampener-safe? report)))))
    (list (part1) (part2))))

(day2)
