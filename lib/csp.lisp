;
; Utility functions:
;

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

(defun cartesian-product (a b)
  (mapcan
    (lambda (item-from-a)
      (mapcar
        (lambda (item-from-b)
          (if (listp item-from-a)
            (append item-from-a (list item-from-b))
            (list item-from-a item-from-b)))
        b))
    a))

(defun print-hash (hash)
  (loop for k being the hash-keys in hash using (hash-value v)
    do (format t "~a => ~a~%" k v)))

(defun concat-string (list)
  (format nil "~{~a~}" list))

(defun my-map-hash (func hash)
  (loop for k being the hash-keys in hash using (hash-value v)
    collect (funcall func k v)))

(defun get-values (hash)
  (my-map-hash (lambda (k v) v) hash))

; snippet from http://stackoverflow.com/a/26061176:
(defun copy-hash-table (hash-table)
  (let ((ht (make-hash-table
             :test (hash-table-test hash-table)
             :rehash-size (hash-table-rehash-size hash-table)
             :rehash-threshold (hash-table-rehash-threshold hash-table)
             :size (hash-table-size hash-table))))
    (loop for key being each hash-key of hash-table
       using (hash-value value)
       do (setf (gethash key ht) value)
       finally (return ht))))

; inspired by snippet from http://stackoverflow.com/a/2087771/1108866:
(defun k-permutations (n-list k)
  (cond ((equal 0 k) (list nil))
        ((null n-list) nil)
        ((null (cdr n-list)) (list n-list))
        (t (loop for element in n-list
             append (mapcar (lambda (l) (cons element l))
                            (k-permutations (remove element n-list) (- k 1)))))))

;
; Class definitions:
;

(defclass constraint ()
  ((scope
      :initarg :scope
      :accessor scope)
    (predicate
      :initarg :predicate
      :accessor predicate)))

(defclass CSP ()
  ((varyables ;variables is a reserved name so we use varyables
    :initarg :varyables
    :accessor varyables)
  (default-domain
    :initarg :default-domain
    :accessor default-domain)
  (domains
    :initarg :domains
    :accessor domains)
  (constraints
    :initarg :constraints
    :accessor constraints)))

(defmethod initialize-instance :after ((self CSP) &key)
  (setf (domains self)
    (mapcar
      (lambda (_) (default-domain self))
      (varyables self))))

; ; TODO: account for bidirectional constraints.  all binary constraints currectly directional
; ; For now, we'll write bi-directional constraints as separate constraints for each way

(defmethod create-n-ary-constraint ((self CSP) varyables predicate &optional z-domain)
  ; create new variable called encap-var1-var2-var3-(length constraints) with domain = cartesian product of domains
  ; add unary constraint on variable
  ; add binary constraints between original variables and ith value of encapsulated variable
  (let* (
    (capsule (concat-string (append (list "encap") varyables)))
    (capsule-domain
      (if (null z-domain)
        (reduce #'cartesian-product (mapcar (lambda (varyable) (domain-for-varyable self varyable)) varyables))
        z-domain))
    (unary-capsule-constraint
      (make-instance 'constraint
        :scope (list capsule)
        :predicate (lambda (tuple) (apply predicate tuple))))
    (binding-constraints-a
      (let ((index 0))
        (mapcar
          (lambda (varyable)
            (incf index)
            (let ((idx index))
              (make-instance 'constraint
                :scope (list varyable capsule)
                :predicate (lambda (original-var encapsulated-var)
                  ; (format t "~%comparing ~a and ~a at ~a" original-var encapsulated-var idx)
                  (equal original-var (nth (- idx 1) encapsulated-var))))))
          varyables)))
    (binding-constraints-b
      (let ((index 0))
        (mapcar
          (lambda (varyable)
            (incf index)
            (let ((idx index))
              (make-instance 'constraint
                :scope (list capsule varyable)
                :predicate (lambda (encapsulated-var original-var)
                  ; (format t "~%comparing ~a and ~a at ~a" original-var encapsulated-var idx)
                  (equal original-var (nth (- idx 1) encapsulated-var))))))
          varyables))))

    (push capsule (varyables self))
    (push capsule-domain (domains self))
    (push unary-capsule-constraint (constraints self))
    (setf (constraints self) (append (constraints self) binding-constraints-a))
    (setf (constraints self) (append (constraints self) binding-constraints-b))))

(defmethod all-binary-constraints ((self CSP))
  (remove-if-not
    (lambda (constraint) (equal 2 (length (scope constraint))))
    (constraints self)))

(defmethod constraints-on ((self CSP) varyable)
  (remove-if
    (lambda (constraint) (equal nil (position varyable (scope constraint))))
    (constraints self)))

(defmethod unary-constraints-on ((self CSP) varyable)
  (remove-if-not
    (lambda (constraint) (equal (length (scope constraint)) 1))
    (constraints-on self varyable)))

(defmethod binary-constraints-on ((self CSP) varyable)
  (remove-if-not
    (lambda (constraint) (equal (length (scope constraint)) 2))
    (constraints-on self varyable)))

; order of arguments matter here
(defmethod binary-constraints-with ((self CSP) varyable-a varyable-b)
  (remove-if-not
    (lambda (constraint)
      (and
        (equal 0 (position varyable-a (scope constraint)))
        (equal 1 (position varyable-b (scope constraint)))))
    (binary-constraints-on self varyable-a)))

(defmethod make-csp-node-consistent ((self CSP))
  (setf (domains self)
    (mapcar
      (lambda (varyable)
        (let (
          (constraints (unary-constraints-on self varyable))
          (domain (domain-for-varyable self varyable)))
          (if (null constraints)
            domain
            (remove-if-not
              (lambda (value)
                (some
                  (lambda (constraint)
                    (funcall (predicate constraint) value))
                  constraints))
              domain))))
      (varyables self))))

;
; Assignment (search state):
;

(defclass assignment ()
  ((assigned-values
    :initarg :assigned-values
    :accessor assigned-values)
  (updated-domains
    :initarg :updated-domains
    :accessor updated-domains)))

(defmethod is-complete ((self assignment))
  (notany #'null (get-values (assigned-values self))))

(defmethod is-consistent ((self assignment) csp)
  (notany #'null
    ; for each constraint:
    ; return T if not all variables in scope are assigned
    ; return T if all variables assigned and predicate passes with values
    ; return F if all variables assigned but predicate fails with values
    (mapcar
      (lambda (constraint)
        (let*
          ((values-for-varyable-in-scope
            (mapcar
              (lambda (varyable) (gethash varyable (assigned-values self)))
              (scope constraint)))
          (all-variables-in-scope-assigned
            (notany #'null values-for-varyable-in-scope)))

          (if all-variables-in-scope-assigned
            (apply (predicate constraint) values-for-varyable-in-scope)
            T)))
      (constraints csp))))

(defmethod is-solution ((self assignment) csp)
  (and
    (is-complete self)
    (is-consistent self csp)))

(defmethod is-dead-end ((self assignment))
  (some #'null
    (mapcar
      (lambda (varyable) (gethash varyable (updated-domains self)))
      (unassigned-varyables self))))

(defmethod copy ((self assignment))
  (make-instance 'assignment
    :assigned-values (copy-hash-table (assigned-values self))
    :updated-domains (copy-hash-table (updated-domains self))))

(defmethod print-assignment ((self assignment))
  (format t "~%assigned values:~%")
  (print-hash (assigned-values self))
  (format t "~%updated domains:~%")
  (print-hash (updated-domains self)))

(defmethod unassigned-varyables ((self assignment))
  (loop for k being the hash-keys in (assigned-values self) using (hash-value v)
    when (null v)
    collect k))

(defmethod varyable-with-min-remaining-values ((self assignment))
  (let*
    ((vars-to-domain-lengths
      (mapcar
        (lambda (varyable) (list varyable (length (gethash varyable (updated-domains self)))))
        (unassigned-varyables self)))
    (sorted-vars-to-domain-lengths
      (sort vars-to-domain-lengths
      (lambda (tuple-a tuple-b)
        (< (second tuple-a) (second tuple-b))))))
    ; (print-hash (assigned-values self))
    ; (print (unassigned-varyables self))
    ; (print sorted-vars-to-domain-lengths)
    (first (first sorted-vars-to-domain-lengths))))

(defmethod index-of-varyable ((self csp) varyable)
  (position varyable (varyables self)))

(defmethod domain-for-varyable ((self csp) varyable)
  (nth (index-of-varyable self varyable) (domains self)))

(defmethod get-domain-for ((self assignment) varyable)
  (gethash varyable (updated-domains self)))

(defmethod set-domain-for ((self assignment) varyable new-domain)
  (setf (gethash varyable (updated-domains self)) new-domain))

(defmethod empty-assignment ((self CSP))
  (let
    ((assigned-values (make-hash-table))
    (updated-domains (make-hash-table)))

    (loop
      for varyable in (varyables self)
      for domain in (domains self) do
        (setf (gethash varyable assigned-values) NIL)
        (setf (gethash varyable updated-domains) domain))

    (make-instance 'assignment
      :assigned-values assigned-values
      :updated-domains updated-domains)))

;
; Arc consistency:
;

(defmethod revise ((self assignment) csp varyable-a varyable-b)
  (let ((changed NIL))
    (loop for constraint in (binary-constraints-with csp varyable-a varyable-b) do
      (let* (
        (domain-a (get-domain-for self varyable-a))
        (domain-b (get-domain-for self varyable-b))
        (new-domain-a
        (remove-if-not
          (lambda (value)
            (some (lambda (other-value) (funcall (predicate constraint) value other-value))
              domain-b))
          domain-a)))
        (unless (equal (length domain-a) (length new-domain-a))
          (setf changed T))
        (set-domain-for self varyable-a new-domain-a)))
    changed))

(defmethod make-arc-consistent ((self assignment) csp)
  ; arcs = scopes of binary constraints
  ; for each arc in arcs: revise ((get-varyable-by-name (first arc)) (get-varyable-by-name (second arc)))
  ; if revise's return value is true, push back on to queue
  (let ((queue (mapcar #'scope (all-binary-constraints csp))))
    (loop until (null queue)
      for arc = (pop queue) do
        (if (revise self csp (first arc) (second arc))
          (loop for constraint-on-other in (binary-constraints-on csp (second arc)) do
            (push (scope constraint-on-other) queue)))))) ; TODO: limit to only where (second arc) is first in scope

;
; backtracking with maintain arc consistency heuristic:
;

(defun backtrack (assignment csp)
  (if (is-complete assignment)
    (return-from backtrack assignment))
  (let ((unassigned-var (varyable-with-min-remaining-values assignment)))
    (loop for value in (gethash unassigned-var (updated-domains assignment)) do
      (format t "~% Assigning ~a to ~a" value unassigned-var)
      (let ((new-assignment (copy assignment)))
        (setf (gethash unassigned-var (assigned-values new-assignment)) value)
        ; constrain the domain to the chosen value
        (setf (gethash unassigned-var (updated-domains new-assignment)) (list value))
        (make-arc-consistent new-assignment csp)
        (if (and (is-consistent new-assignment csp) (not (is-dead-end new-assignment)))
          (let ((result (backtrack new-assignment csp)))
            (if (not (null result))
              (return-from backtrack result))))
        ))
    NIL))

(defmethod backtracking-search ((self CSP))
  (let ((assignment (empty-assignment self)))
    (make-arc-consistent assignment self)
    ; (print-assignment assignment)
    (backtrack assignment self)))

;
; Problem definition:
;

(defparameter *pqrs*
  (make-instance 'CSP
    :varyables '(p q r s tt w g h j c1 c2 c3 c4 c5)
    :default-domain '(0 1 2 3 4 5 6 7 8 9)
    :constraints (list
      (make-instance 'constraint :scope '(q) :predicate #'evenp)
      (make-instance 'constraint :scope '(w) :predicate (lambda (w) (> w 0)))
      (make-instance 'constraint :scope '(w c5) :predicate (lambda (w c5) (equal w c5)))
      (make-instance 'constraint :scope '(c5 w) :predicate (lambda (c5 w) (equal w c5))))))

; make node-consistent before applying n-ary constraints so their encapsulated variables aren't unneccesarily big
(make-csp-node-consistent *pqrs*)

(create-n-ary-constraint *pqrs* '(p q r s tt w g h j) #'all-diff (k-permutations (default-domain *pqrs*) 9))
(create-n-ary-constraint *pqrs* '(tt q c1) (lambda (tt q c1) (equal (+ tt tt) (+ q (* c1 10)))))
(create-n-ary-constraint *pqrs* '(s c1 j c2) (lambda (s c1 j c2) (equal (+ s s c1) (+ j (* c2 10)))))
(create-n-ary-constraint *pqrs* '(r c2 tt c3) (lambda (r c2 tt c3) (equal (+ r r c2) (+ tt (* c3 10)))))
(create-n-ary-constraint *pqrs* '(q c3 h c4) (lambda (q c3 h c4) (equal (+ q q c3) (+ h (* c4 10)))))
(create-n-ary-constraint *pqrs* '(p w c4 g c5) (lambda (p w c4 g c5) (equal (+ p w c4) (+ g (* c5 10)))))


; then, apply the unary constraints on new intermediate variables:
(make-csp-node-consistent *pqrs*)

(let ((result (backtracking-search *pqrs*)))
  (if (not (null result))
    (print-assignment result)
    (print "no solution found")))
