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

(setf variables '(a b c d))
(setf domain '(0 1 2 3 4 5 6 7 8 9))
(setf constraints
  '(
    ((a b c d) equal-sums)
    ((a b c d) all-diff)))

(print variables)
(print constraints)
(print (all-diff 3 4 9 1 8 5 3))

(defun verify (constraint)
  (let (
    (variables (first constraint))
    (predicate (second constraint)))
    (apply predicate (mapcar #'symbol-value variables))))

; test verify:
(setf a 5)
(setf b 3)
(setf c 7)
(setf d 1)

(print (verify (first constraints)))
(print (verify (second constraints)))