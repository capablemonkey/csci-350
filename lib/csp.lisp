(defun equal-sums (a b c d)
  (equal (+ a b) (+ c d)))

(defun all-diff (&rest list)
  (let ((sorted-list (sort list #'<)))
    (loop
      until (null sorted-list)
      do
        (if (equal (first sorted-list) (second sorted-list)) (return-from all-diff NIL))
        (setf sorted-list (rest sorted-list)))
    T))

(print (all-diff 3 4 9 1 8 5 3))

(setf variables '(a b c d))
(setf domain '(0 1 2 3 4 5 6 7 8 9))
(setf constraints
  '(
    ((a b c d) equal-sums)
    ((a b c d) all-diff)
    ((a) oddp)))

(defun verify (constraint assignments)
  (let* (
    (variables (first constraint))
    (predicate (second constraint))
    (values (mapcar (lambda (key) (gethash key assignments)) variables))
    (satisfied? (apply predicate values)))
      (format t "~%* constraint ~a satisfied? [~a]" constraint satisfied?)
      satisfied?))

(defun verify-constraints (constraints assignments)
  (every (lambda (constraint) (verify constraint assignments)) constraints))

; test verify:

; (setf assignments (make-hash-table))
; (setf (gethash 'a assignments) 3)
; (setf (gethash 'b assignments) 5)
; (setf (gethash 'c assignments) 7)
; (setf (gethash 'd assignments) 1)

; (print (verify-constraints constraints assignments))

(defun brute-force (domain constraints)
  (loop for a in domain do
    (loop for b in domain do
      (let ((assignments (make-hash-table)))
        (setf (gethash 'a assignments) a)
        (setf (gethash 'b assignments) b)
        (setf (gethash 'c assignments) 7)
        (setf (gethash 'd assignments) 1)
        (if (verify-constraints constraints assignments)
          (return-from brute-force assignments)))
      )))

(let ((results (brute-force domain constraints)))
  (if results
    (format t "~%Solution found: a: ~a | b: ~a | c: ~a | d: ~a"
      (gethash 'a results)
      (gethash 'b results)
      (gethash 'c results)
      (gethash 'd results))
    (format t "~%No solution found")))
