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

(defmethod make-arc-consistent ((self assignment))
  ())

(defmethod copy ((self assignment))
  (make-instance 'assignment
    :assigned-values (assigned-values self)
    :updated-domains (updated-domains self)))

; (defmethod domain-for-varyable ((self assignment) (csp CSP) varyable)
;   (nth (position varyable (varyables csp)) (updated-domains self)))

(defmethod domain-for-varyable ((self csp) varyable)
  (nth (position varyable (varyables self)) (domains self)))

(defmethod empty-assignment ((self CSP))
  (make-instance 'assignment
    :assigned-values (make-list (length (varyables self)))
    :updated-domains (domains self)))

(defun backtracking-search (self CSP)
  (backtrack (empty-assignment self)))

(defun backtrack (assignment csp)
  ())

(defparameter *two-four*
  (make-instance 'CSP
    :varyables '(tt w o ff u r c1 c2 c3)
    :default-domain '(0 1 2 3 4 5 6 7 8 9)
    :constraints (list
      (make-instance 'constraint :scope '(r) :predicate #'evenp)
      (make-instance 'constraint :scope '(ff c3) :predicate (lambda (ff c3) (equal ff c3)))
      (make-instance 'constraint :scope '(c3 ff) :predicate (lambda (c3 ff) (equal ff c3))))))

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

; (defmethod binary-constraints-on ((self varyable))
;   (remove-if-not
;     (lambda (constraint) (equal (length (scope constraint)) 2))
;     (constraints-on self var)))

; ; order of arguments matter here
; (defmethod binary-constraints-with ((self varyable) (other varyable))
;   (remove-if-not
;     (lambda (constraint)
;       (and
;         (equal 0 (position (name self) (scope constraint)))
;         (equal 1 (position (name other) (scope constraint)))))
;     (binary-constraints-on self)))

; (defmethod revise ((self varyable) (other varyable))
;   (let ((changed NIL))
;     (loop for constraint in (binary-constraints-with self other) do
;       (let ((new-domain
;         (remove-if-not
;           (lambda (value)
;             (some (lambda (other-value) (funcall (predicate constraint) value other-value))
;               (domain other)))
;           (domain self))))
;         (unless (equal (length (domain self)) (length new-domain))
;           (setf changed T))
;         (setf (domain self) new-domain)))
;     changed))

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

(make-csp-node-consistent *two-four*)
(create-n-ary-constraint *two-four* '(tt w o ff u r) #'all-diff)
(create-n-ary-constraint *two-four* '(o r c1) (lambda (o r c1) (equal (+ o o) (+ r (* c1 10)))))
(create-n-ary-constraint *two-four* '(w c1 u c2) (lambda (w c1 u c2) (equal (+ w w (* c1 10)) (+ u (* c2 10)))))
(create-n-ary-constraint *two-four* '(tt c2 o ff) (lambda (tt c2 o ff) (equal (+ tt tt (* c2 10)) (+ o (* ff 10)))))

(make-csp-node-consistent *two-four*)

; (defmethod make-csp-arc-consistent ((self CSP))
;   ; arcs = scopes of binary constraints
;   ; for each arc in arcs: revise ((get-varyable-by-name (first arc)) (get-varyable-by-name (second arc)))
;   ; if revise's return value is true, push back on to queue
;   (let ((queue (mapcar #'scope (all-binary-constraints))))
;     (loop until (null queue)
;       for arc = (pop queue) do
;         (if (revise (get-varyable-by-name (first arc)) (get-varyable-by-name (second arc)))
;           (loop for constraint-on-other in (binary-constraints-on (get-varyable-by-name (second arc))) do
;             (push (scope constraint-on-other) queue))))))

; (make-csp-arc-consistent)

; ; (revise (get-varyable-by-name 'q) (get-varyable-by-name "encapQWU"))
; ; (revise (get-varyable-by-name 'w) (get-varyable-by-name "encapQWU"))
; ; (revise (get-varyable-by-name 'u) (get-varyable-by-name "encapQWU"))
; ; (revise (get-varyable-by-name 'w) (get-varyable-by-name "encapQWU"))

; (loop for c in *constraints* do (describe c))
; (loop for v in *varyables* do (describe v))

; ; (make-node-consistent (get-varyable-by-name "encapQWU"))
; ; (print (domain (get-varyable-by-name "encapQWU")))
; ; (describe (unary-constraints-on (get-varyable-by-name "encapQWU")))

(let ((assignment (empty-assignment *two-four*)))
  (describe assignment)
  (print (updated-domains assignment)))
