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

; variable is a reserved symbol so we use varyable
(defclass varyable ()
  ((name :initarg :name :accessor name)
    (domain :initarg :domain :accessor domain)))

(defclass constraint ()
  ((scope :initarg :scope :accessor scope)
    (predicate :initarg :predicate :accessor predicate)))

(defparameter *domain* '(0 1 2 3 4 5 6 7 8 9))

(defparameter *varyables*
  (list
    (make-instance 'varyable :name 'tt :domain *domain*)
    (make-instance 'varyable :name 'w :domain *domain*)
    (make-instance 'varyable :name 'o :domain *domain*)
    (make-instance 'varyable :name 'ff :domain *domain*)
    (make-instance 'varyable :name 'u :domain *domain*)
    (make-instance 'varyable :name 'r :domain *domain*)
    (make-instance 'varyable :name 'c1 :domain *domain*)
    (make-instance 'varyable :name 'c2 :domain *domain*)
    (make-instance 'varyable :name 'c3 :domain *domain*)))

(defparameter *constraints*
  (list
    (make-instance 'constraint :scope '(r) :predicate #'evenp)
    (make-instance 'constraint :scope '(ff c3) :predicate (lambda (ff c3) (equal ff c3)))
    (make-instance 'constraint :scope '(c3 ff) :predicate (lambda (c3 ff) (equal ff c3)))
    ))

; TODO: account for bidirectional constraints.  all binary constraints currectly directional
; For now, we'll write bi-directional constraints as separate constraints for each way

(defun create-n-ary-constraint (varyable-names predicate)
  ; create new variable called encap-var1-var2-var3-(length constraints) with domain = cartesian product of domains
  ; add unary constraint on variable
  ; add binary constraints between original variables and ith value of encapsulated variable
  (let* (
    (varyables (mapcar #'get-varyable-by-name varyable-names))
    (capsule
      (make-instance 'varyable
        :name (concat-string (append (list "encap") varyable-names))
        :domain (reduce #'cartesian-product (mapcar #'domain varyables))))
    (unary-capsule-constraint
      (make-instance 'constraint
        :scope (list (name capsule))
        :predicate (lambda (tuple) (apply predicate tuple))))
    (binding-constraints-a
      (let ((index 0))
        (mapcar
          (lambda (varyable)
            (incf index)
            (let ((idx index))
              (make-instance 'constraint
                :scope (list (name varyable) (name capsule))
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
                :scope (list (name capsule) (name varyable))
                :predicate (lambda (encapsulated-var original-var)
                  ; (format t "~%comparing ~a and ~a at ~a" original-var encapsulated-var idx)
                  (equal original-var (nth (- idx 1) encapsulated-var))))))
          varyables))))

    (push capsule *varyables*)
    (push unary-capsule-constraint *constraints*)
    (setf *constraints* (append *constraints* binding-constraints-a))
    (setf *constraints* (append *constraints* binding-constraints-b))))

(defun all-binary-constraints ()
  (remove-if-not
    (lambda (constraint) (equal 2 (length (scope constraint))))
    *constraints*))

(defmethod constraints-on ((self varyable))
  (remove-if
    (lambda (constraint) (equal nil (position (name self) (scope constraint))))
    *constraints*))

(defmethod unary-constraints-on ((self varyable))
  (remove-if-not
    (lambda (constraint) (equal (length (scope constraint)) 1))
    (constraints-on self)))

(defmethod binary-constraints-on ((self varyable))
  (remove-if-not
    (lambda (constraint) (equal (length (scope constraint)) 2))
    (constraints-on self)))

; order of arguments matter here
(defmethod binary-constraints-with ((self varyable) (other varyable))
  (remove-if-not
    (lambda (constraint)
      (and
        (equal 0 (position (name self) (scope constraint)))
        (equal 1 (position (name other) (scope constraint)))))
    (binary-constraints-on self)))

(defmethod make-node-consistent ((self varyable))
  (loop for constraint in (unary-constraints-on self) do
    (setf (domain self)
      (remove-if-not
        (lambda (val) (funcall (predicate constraint) val))
        (domain self)))))

(defmethod revise ((self varyable) (other varyable))
  (let ((changed NIL))
    (loop for constraint in (binary-constraints-with self other) do
      (let ((new-domain
        (remove-if-not
          (lambda (value)
            (some (lambda (other-value) (funcall (predicate constraint) value other-value))
              (domain other)))
          (domain self))))
        (unless (equal (length (domain self)) (length new-domain))
          (setf changed T))
        (setf (domain self) new-domain)))
    changed))

(defun get-varyable-by-name (name)
  (loop for varyable in *varyables* do
    (if (equal name (name varyable))
      (return-from get-varyable-by-name varyable)))
  nil)

(defun make-csp-node-consistent ()
  (loop for varyable in *varyables* do
    (make-node-consistent varyable)))

(make-csp-node-consistent)
(create-n-ary-constraint '(tt w o ff u r) #'all-diff)
(create-n-ary-constraint '(o r c1) (lambda (o r c1) (equal (+ o o) (+ r (* c1 10)))))
(create-n-ary-constraint '(w c1 u c2) (lambda (w c1 u c2) (equal (+ w w (* c1 10)) (+ u (* c2 10)))))
(create-n-ary-constraint '(tt c2 o ff) (lambda (tt c2 o ff) (equal (+ tt tt (* c2 10)) (+ o (* ff 10)))))

(make-csp-node-consistent)

(defun make-csp-arc-consistent ()
  ; arcs = scopes of binary constraints
  ; for each arc in arcs: revise ((get-varyable-by-name (first arc)) (get-varyable-by-name (second arc)))
  ; if revise's return value is true, push back on to queue
  (let ((queue (mapcar #'scope (all-binary-constraints))))
    (loop until (null queue)
      for arc = (pop queue) do
        (if (revise (get-varyable-by-name (first arc)) (get-varyable-by-name (second arc)))
          (loop for constraint-on-other in (binary-constraints-on (get-varyable-by-name (second arc))) do
            (push (scope constraint-on-other) queue))))))

(make-csp-arc-consistent)

; (revise (get-varyable-by-name 'q) (get-varyable-by-name "encapQWU"))
; (revise (get-varyable-by-name 'w) (get-varyable-by-name "encapQWU"))
; (revise (get-varyable-by-name 'u) (get-varyable-by-name "encapQWU"))
; (revise (get-varyable-by-name 'w) (get-varyable-by-name "encapQWU"))

(loop for c in *constraints* do (describe c))
(loop for v in *varyables* do (describe v))

; (make-node-consistent (get-varyable-by-name "encapQWU"))
; (print (domain (get-varyable-by-name "encapQWU")))
; (describe (unary-constraints-on (get-varyable-by-name "encapQWU")))

; (defclass CSP ()
;   (variables)
;   (assignment)
;   (domain)
;   (constraints))
