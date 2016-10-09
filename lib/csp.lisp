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

(setf variables '(p q r s tt w g h j c1 c2 c3 c4 c5))
(setf domain '(0 1 2 3 4 5 6 7 8 9))
(setf constraints
  (list
    (list '(q) #'evenp)
    (list '(w) (lambda (w) (> w 0)))
    (list '(w c5) (lambda (ff c5) (equal ff c5)))
    (list '(tt q c1) (lambda (tt q c1) (equal (+ tt tt) (+ q (* c1 10)))))
    (list '(s c1 j c2) (lambda (s c1 j c2) (equal (+ s s c1) (+ j (* c2 10)))))
    (list '(r c2 tt c3) (lambda (r c2 tt c3) (equal (+ r r c2) (+ tt (* c3 10)))))
    (list '(q c3 h c4) (lambda (q c3 h c4) (equal (+ q q c3) (+ h (* c4 10)))))
    (list '(p w c4 g c5) (lambda (p w c4 g c5) (equal (+ p w c4) (+ g (* c5 10)))))
    (list '(p q r s tt w g h j) (lambda (&rest vars) (apply 'all-diff vars)))
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
  (loop for p in domain do
    (loop for q in domain do
      (loop for r in domain do
        (loop for s in domain do
          (loop for tt in domain do
            (loop for w in domain do
              (loop for g in domain do
                (loop for h in domain do
                  (loop for j in domain do
                    (let ((assignments (make-hash-table)))
                      (setf (gethash 'p assignments) p)
                      (setf (gethash 'q assignments) q)
                      (setf (gethash 'r assignments) r)
                      (setf (gethash 's assignments) s)
                      (setf (gethash 'tt assignments) tt)
                      (setf (gethash 'w assignments) w)
                      (setf (gethash 'g assignments) g)
                      (setf (gethash 'h assignments) h)
                      (setf (gethash 'j assignments) j)
                      (setf (gethash 'c1 assignments) (carry (+ tt tt) 10))
                      (setf (gethash 'c2 assignments) (carry (+ s s (gethash 'c1 assignments)) 10))
                      (setf (gethash 'c3 assignments) (carry (+ r r (gethash 'c2 assignments)) 10))
                      (setf (gethash 'c4 assignments) (carry (+ q q (gethash 'c3 assignments)) 10))
                      (setf (gethash 'c5 assignments) (carry (+ p w (gethash 'c4 assignments)) 10))
                      (if (verify-constraints constraints assignments)
                        (return-from brute-force assignments)))
      ))))))))))

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
