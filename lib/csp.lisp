(defun carry (a b)
  (multiple-value-bind (quot rem) (floor a b)
    quot))

(defun all-diff (&rest list)
  (let ((sorted-list (sort list #'<)))
    (loop
      until (null sorted-list)
      do
        (if (equal (first sorted-list) (second sorted-list)) (return-from all-diff NIL))
        (setf sorted-list (rest sorted-list)))
    T))

; (print (all-diff 3 4 9 1 8 5 3))

(setf variables '(tt w o ff u r c1 c2 c3))
(setf domain '(0 1 2 3 4 5 6 7 8 9))
(setf constraints
  (list
    (list '(r) #'evenp)
    (list '(tt w o ff u r) (lambda (&rest vars) (apply 'all-diff vars)))
    (list '(o r c1) (lambda (o r c1) (equal (+ o o) (+ r (* c1 10)))))
    (list '(w c1 u c2) (lambda (w c1 u c2) (equal (+ w w (* c1 10)) (+ u (* c2 10)))))
    (list '(tt c2 o ff) (lambda (tt c2 o ff) (equal (+ tt tt (* c2 10)) (+ o (* ff 10)))))
    (list '(ff) (lambda (ff) (> ff 0)))
    (list '(ff c3) (lambda (ff c3) (equal ff c3)))
    ))

(defun verify (constraint assignments)
  (let* (
    (variables (first constraint))
    (predicate (second constraint))
    (values (mapcar (lambda (key) (gethash key assignments)) variables))
    (satisfied? (apply predicate values)))
      ; (format t "~%* constraint ~a satisfied? [~a]" constraint satisfied?)
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
  (loop for tt in domain do
    (loop for w in domain do
      (loop for o in domain do
        (loop for ff in domain do
          (loop for u in domain do
            (loop for r in domain do
              (let ((assignments (make-hash-table)))
                (setf (gethash 'tt assignments) tt)
                (setf (gethash 'w assignments) w)
                (setf (gethash 'o assignments) o)
                (setf (gethash 'ff assignments) ff)
                (setf (gethash 'u assignments) u)
                (setf (gethash 'r assignments) r)
                (setf (gethash 'c1 assignments) (carry (+ o o) 10))
                (setf (gethash 'c2 assignments) (carry (+ w w (gethash 'c1 assignments)) 10))
                (setf (gethash 'c3 assignments) (carry (+ tt tt (gethash 'c2 assignments)) 10))
                (if (verify-constraints constraints assignments)
                  (return-from brute-force assignments)))
      )))))))

(let ((results (brute-force domain constraints)))
  (if results
    (format t "~%Solution found: tt: ~a | w: ~a | o: ~a | ff: ~a | u: ~a | r: ~a"
      (gethash 'tt results)
      (gethash 'w results)
      (gethash 'o results)
      (gethash 'ff results)
      (gethash 'u results)
      (gethash 'r results))
    (format t "~%No solution found")))
