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

; variable is a reserved symbol so we use varyable
(defclass varyable ()
  ((name :initarg :name :accessor name)
    (domain :initarg :domain :accessor domain)))

; (defmethod initialize-instance :after ((self varyable) &key)
;   (setf (name self) self)
;   (name self))

(defclass encapsulated-varyable (varyable)
  ((target-varyables :initarg :target-varyables :accessor target-varyables)))

(defclass constraint ()
  ((scope :initarg :scope :accessor scope)
    (predicate :initarg :predicate :accessor predicate)))

(defparameter *domain* '(0 1 2 3 4 5 6 7 8 9))

(defparameter *varyables*
  (list
    (make-instance 'varyable :name 'q :domain *domain*)
    (make-instance 'varyable :name 'w :domain *domain*)))

(defparameter *constraints*
  (list
    (make-instance 'constraint :scope '(q) :predicate #'evenp)
    (make-instance 'constraint :scope '(w) :predicate (lambda (w) (> w 6)))
    (make-instance 'constraint :scope '(w q) :predicate (lambda (w q) (> q w)))
    ))

(defmethod constraints ((self varyable))
  (remove-if
    (lambda (constraint) (equal nil (position (name self) (scope constraint))))
    *constraints*))

(defmethod unary-constraints ((self varyable))
  (remove-if
    (lambda (constraint) (> (length (scope constraint)) 1))
    (constraints self)))

(defmethod make-node-consistent ((self varyable))
  (loop for constraint in (unary-constraints self) do
    (setf (domain self)
      (remove-if-not
        (lambda (val) (funcall (predicate constraint) val))
        (domain self)))))

(print (constraints (first *varyables*)))

(make-node-consistent (second *varyables*))
(print (domain (second *varyables*)))

; (defclass CSP ()
;   (variables)
;   (assignment)
;   (domain)
;   (constraints))
