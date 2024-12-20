
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

(defun rows (str)
  (loop for line in (lines str)
        collect (mapcar #'parse-integer (cl-ppcre:split "[ ]+" line))))

(defun columns (str)
  (transpose (rows str)))

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
  (flet ((part1 ()
           (loop for report in (rows input)
                 counting (safe-report? report)))
         (part2 ()
           (loop for report in (rows input)
                 for safe = (safe-report? report)
                 counting (or safe (problem-dampener-safe? report)))))
    (list (part1) (part2))))

(day2)

;; Day 3

(defvar sample3 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defun day3-part1 (input)
  (let ((sum 0)
        (mults ()))
    (cl-ppcre:do-register-groups (x y) ("mul\\(([0-9]{1,3}),([0-9]{1,3})\\)" input)
      (check-type x string)
      (check-type y string)
      (push (cons (parse-integer x) (parse-integer y)) mults)
      (incf sum (* (parse-integer x) (parse-integer y))))
    sum))

(day3-part1 *day3-input*) ;9:30

(defvar sample3-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defun num-char? (c)
  (<= (char-code #\0) (char-code c) (char-code #\9)))

(defun scan-mem-for-mults (str)
  (let ((enabled? t)
        (mults ())
        (mult-scan? nil)
        (last-mult-char #\0)
        (mult-num-scan? nil)
        (mult-num "")
        (mult-pair-added? nil)
        (do-scan? nil)
        (last-do-char #\0)
        (dont-scan? nil)
        (last-dont-char #\0))
    (loop for char across str
          do
          (cond ((and (eql #\m char) (not mult-scan?) enabled?)
                 (setf mult-scan? t
                       mult-pair-added? nil
                       last-mult-char #\m))
                (mult-scan?
                  (cond ((and (eql #\m last-mult-char) (eql #\u char))
                         (setf last-mult-char #\u))
                        ((and (eql #\u last-mult-char) (eql #\l char))
                         (setf last-mult-char #\l))
                        ((and (eql #\l last-mult-char) (eql #\( char))
                         (setf last-mult-char #\())
                        ((and (eql #\( last-mult-char) (num-char? char))
                         (setf mult-num (uiop:strcat mult-num char))
                         (setf mult-num-scan? t))
                        (mult-num-scan?
                          (cond ((num-char? char)
                                 (setf mult-num (uiop:strcat mult-num char)))
                                ((eql #\, char)
                                 (push (cons (parse-integer mult-num) nil) mults)
                                 (setf mult-num ""
                                       mult-pair-added? t))
                                ((eql #\) char)
                                 (setf (cdr (first mults)) (parse-integer mult-num)
                                       mult-num ""
                                       mult-num-scan? nil
                                       mult-scan? nil))
                                (t
                                 (when mult-pair-added?
                                   (pop mults))
                                 (setf mult-num ""
                                       mult-num-scan? nil
                                       mult-scan? nil))))
                        (t
                         (setf mult-scan? nil))))
                ((and (eql #\d char) (not do-scan?) (not dont-scan?))
                 (setf do-scan? t
                       dont-scan? t
                       last-do-char #\d
                       last-dont-char #\d))
                ((or do-scan? dont-scan?)
                 (cond ((and (eql #\d last-do-char) (eql #\o char))
                        (setf last-do-char #\o
                              last-dont-char #\o))
                       ((and (eql #\o last-do-char) (eql #\o last-dont-char) (eql #\( char))
                        (setf last-do-char #\(
                              dont-scan? nil))
                       ((and (eql #\( last-do-char) (eql #\) char))
                        (setf do-scan? nil
                              dont-scan? nil
                              enabled? t))
                       ((and (eql #\o last-dont-char) (eql #\n char))
                        (setf do-scan? nil
                              last-dont-char #\n))
                       ((and (eql #\n last-dont-char) (eql #\' char))
                        (setf last-dont-char #\'))
                       ((and (eql #\' last-dont-char) (eql #\t char))
                        (setf last-dont-char #\t))
                       ((and (eql #\t last-dont-char) (eql #\( char))
                        (setf last-dont-char #\())
                       ((and (eql #\( last-dont-char) (eql #\) char))
                        (setf dont-scan? nil
                              enabled? nil))
                       (t
                        (setf do-scan? nil
                              dont-scan? nil))))))
    (loop for (x . y) in mults
          summing (* x y))))

; part 2:
(scan-mem-for-mults sample3-2)
(scan-mem-for-mults *day3-input*)
#|
It took 2:10:00 total, so basically 2 extra hours from part 1...
I had the sense that part 2 could be solved with regex, too, but I thought maybe I'll just write a parser...
And then I wrote the worst parser ever above, but it's basically a pure char-by-char state machine, so it's kind of cool.
I got stuck for a while with the wrong answer on the full input though.
I should have first compared part1 with part2 but ignoring whether enabled? was true or false, I was getting different numbers.
I found that these:

(day3-part1 "mul(108,347)how()]!(@[&{}where()mul(345,953)&who(717,992):%what()<mul(566{;>>}^!why()*mul(840,962)what()")
(scan-mem-for-mults "mul(108,347)how()]!(@[&{}where()mul(345,953)&who(717,992):%what()<mul(566{;>>}^!why()*mul(840,962)what()")

were getting different results.
Basically my part 2 solution was too greedily pop'ing on bad input. That mul(566{ resulted in popping the previous one, because
566 hadn't been added yet. I added yet another state var to handle the case to only pop when a new pair has been added.

Maybe I'll be lucky with subsequent days and this state machine approach will work out better than regexes in the long run...
|#


;; day 4

(defvar sample4 "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

;(import 'alexandria:if-let)

(defun char-at-row-col (lines r c)
  (if (< -1 r (length lines))
      (let ((row (elt lines r)))
        (if (< -1 c (length row))
            (elt row c)))))

(defun paths-around-row-col (r c len)
  "Given a starting row, col pair, give the offsets for each of the 8 directions around it up to len.
   e.g. if 0,0 and 4, the path to the right will be ((0 0) (0 1) (0 2) (0 3)).
   There will be 8 such paths."

  (let
    (
     (right-c  (1- c))
     (down-r  (1- r))
     (left-c  (1+ c))
     (up-r  (1+ r))
     (up-right-r  (1+ r))
     (up-right-c  (1- c))
     (down-right-r  (1- r))
     (down-right-c  (1- c))
     (down-left-r  (1- r))
     (down-left-c  (1+ c))
     (up-left-r  (1+ r))
     (up-left-c  (1+ c))
     )
    (list
      (loop repeat len
            collect (list r (incf right-c)))
      (loop repeat len
            collect (list (incf down-r) c))
      (loop repeat len
            collect (list r (decf left-c)))
      (loop repeat len
            collect (list (decf up-r) c))
      (loop repeat len
            collect (list (decf up-right-r) (incf up-right-c)))
      (loop repeat len
            collect (list (incf down-right-r) (incf down-right-c)))
      (loop repeat len
            collect (list (incf down-left-r) (decf down-left-c)))
      (loop repeat len
            collect (list (decf up-left-r) (decf up-left-c))))))

(coerce (list #\a #\b) 'string)
(loop for path in (paths-around-row-col 4 4 9)
      collect (coerce (loop for (row col) in path
                            for char = (char-at-row-col (lines sample4) row col)
                            when char
                            collect char)
                      'string))

(defun day4-part1 (input &aux (word "XMAS") (word-len (length word)))
  (let ((lines (lines input))
        (sum 0))
    (loop for r from 0 below (length lines)
          for the-row = (elt lines r)
          for row-len = (length the-row)
          do
          (loop for c from 0 below row-len
                do
                (when (eql (elt word 0) (char-at-row-col lines r c))
                  (dolist (connected-word (loop for path in (paths-around-row-col r c word-len)
                                                 collect (coerce (loop for (row col) in path
                                                                       for char = (char-at-row-col lines row col)
                                                                       when char
                                                                       collect char)
                                                                 'string)))
                    (when (string-equal word connected-word)
                      (incf sum))))))
    sum))


(day4-part1 sample4)
(day4-part1 *day4-input*) ; 1 hr 20 min

(defun day4-part2 (input)
  "strategy: do a search again, but now search for 'A', and check if both diagonals form mas / sam"
  (let* ((lines (lines input))
         (line-length (length lines))
         (mas (coerce "MAS" 'list))
         (sam (coerce "SAM" 'list))
         (sum 0))
    (loop for r from 0 below line-length
          for the-row = (elt lines r)
          for row-len = (length the-row)
          do
          (loop for c from 0 below row-len
                do
                (let ((diag1 (list (char-at-row-col lines (1- r) (1- c))
                                   (char-at-row-col lines r c)
                                   (char-at-row-col lines (1+ r) (1+ c))))
                      (diag2 (list (char-at-row-col lines (1- r) (1+ c))
                                   (char-at-row-col lines r c)
                                   (char-at-row-col lines (1+ r) (1- c)))))
                  (when (and (or (equal mas diag1) (equal sam diag1))
                             (or (equal mas diag2) (equal sam diag2)))
                    (incf sum)))))
    sum))


(day4-part2 sample4)
(day4-part2 *day4-input*) ; 15 mins
