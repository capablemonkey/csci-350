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

(defun concat-string (list)
  (format nil "~{~a~}" list))

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

(defclass assignment ()
  ((assigned-values
    :initarg :assigned-values
    :accessor assigned-values)
  (updated-domains
    :initarg :updated-domains
    :accessor updated-domains)))

(defun my-map-hash (func hash)
  (loop for k being the hash-keys in hash using (hash-value v)
    collect (funcall func k v)))

(defun get-values (hash)
  (my-map-hash (lambda (k v) v) hash))

(defmethod is-complete ((self assignment))
  (notany #'null (get-values (assigned-values self))))

(defmethod is-consistent ((self assignment) csp)
  (
  ; get list of assigned variables
  ; get list of constraints between assigned variables
  ; for each constraint, ensure satisifed
    ))

(defmethod is-solution ((self assignment) csp)
  (and
    (is-complete self)
    (is-consistent self csp)))

(defmethod copy ((self assignment))
  (make-instance 'assignment
    :assigned-values (assigned-values self)
    :updated-domains (updated-domains self)))

(defmethod print-assignment ((self assignment))
  (format t "~%assigned values:~%")
  (loop for k being the hash-keys in (assigned-values self) using (hash-value v)
    do (format t "~a => ~a~%" k v))

  (format t "~%updated domains:~%")

  (loop for k being the hash-keys in (updated-domains self) using (hash-value v)
    do (format t "~a => ~a~%" k v)))

(defmethod varyable-with-min-remaining-values ((self assignment))
  (let*
    ((vars-to-domain-lengths
      (my-map-hash
        (lambda (varyable domain) (list varyable (length domain)))
        (updated-domains self)))
    (sorted-vars-to-domain-lengths
      (sort vars-to-domain-lengths
      (lambda (tuple-a tuple-b)
        (< (second tuple-a) (second tuple-b))))))
    (first (first sorted-vars-to-domain-lengths))))

(defmethod index-of-varyable ((self csp) varyable)
  (position varyable (varyables self)))

(defmethod domain-for-varyable ((self csp) varyable)
  (nth (index-of-varyable self varyable) (domains self)))

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

(defmethod get-domain-for ((self assignment) varyable)
  (gethash varyable (updated-domains self)))

(defmethod set-domain-for ((self assignment) varyable new-domain)
  (setf (gethash varyable (updated-domains self)) new-domain))

; ; TODO: account for bidirectional constraints.  all binary constraints currectly directional
; ; For now, we'll write bi-directional constraints as separate constraints for each way

(defmethod create-n-ary-constraint ((self CSP) varyables predicate)
  ; create new variable called encap-var1-var2-var3-(length constraints) with domain = cartesian product of domains
  ; add unary constraint on variable
  ; add binary constraints between original variables and ith value of encapsulated variable
  (let* (
    (capsule (concat-string (append (list "encap") varyables)))
    (capsule-domain (reduce #'cartesian-product (mapcar (lambda (varyable) (domain-for-varyable self varyable)) varyables)))
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

(defmethod make-csp-arc-consistent ((self assignment) csp)
  ; arcs = scopes of binary constraints
  ; for each arc in arcs: revise ((get-varyable-by-name (first arc)) (get-varyable-by-name (second arc)))
  ; if revise's return value is true, push back on to queue
  (let ((queue (mapcar #'scope (all-binary-constraints csp))))
    (loop until (null queue)
      for arc = (pop queue) do
        (if (revise self csp (first arc) (second arc))
          (loop for constraint-on-other in (binary-constraints-on csp (second arc)) do
            (push (scope constraint-on-other) queue)))))) ; TODO: limit to only where (second arc) is first in scope

(defun backtrack (assignment csp)
  (if (is-complete assignment)
    (return-from backtrack assignment))
  (let ((unassigned-var (varyable-with-min-remaining-values assignment)))
    (print unassigned-var)
    (print-assignment assignment)

    ; (for value in () do)
    ; don't forget to use (copy assignment)
    ))

(defmethod backtracking-search ((self CSP))
  (let ((assignment (empty-assignment self)))
    (make-csp-arc-consistent assignment self)
    (backtrack assignment self)))

(defparameter *two-four*
  (make-instance 'CSP
    :varyables '(tt w o ff u r c1 c2 c3)
    :default-domain '(0 1 2 3 4 5 6 7 8 9)
    :constraints (list
      (make-instance 'constraint :scope '(r) :predicate #'evenp)
      (make-instance 'constraint :scope '(ff c3) :predicate (lambda (ff c3) (equal ff c3)))
      (make-instance 'constraint :scope '(c3 ff) :predicate (lambda (c3 ff) (equal ff c3))))))

; make node-consistent before applying n-ary constraints so their encapsulated variables aren't unneccesarily big
(make-csp-node-consistent *two-four*)
(create-n-ary-constraint *two-four* '(tt w o ff u r) #'all-diff)
(create-n-ary-constraint *two-four* '(o r c1) (lambda (o r c1) (equal (+ o o) (+ r (* c1 10)))))
(create-n-ary-constraint *two-four* '(w c1 u c2) (lambda (w c1 u c2) (equal (+ w w (* c1 10)) (+ u (* c2 10)))))
(create-n-ary-constraint *two-four* '(tt c2 o ff) (lambda (tt c2 o ff) (equal (+ tt tt (* c2 10)) (+ o (* ff 10)))))

; then, apply the unary constraints on new intermediate variables:
(make-csp-node-consistent *two-four*)

(backtracking-search *two-four*)
