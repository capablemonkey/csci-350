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
  (remove-if-not
    (lambda (constraint) (equal (length (scope constraint)) 1))
    (constraints self)))

(defmethod binary-constraints ((self varyable))
  (remove-if-not
    (lambda (constraint) (equal (length (scope constraint)) 2))
    (constraints self)))

; order matters here
(defmethod binary-constraints-with ((self varyable) (other varyable))
  (remove-if-not
    (lambda (constraint)
      (and
        (equal 0 (position (name self) (scope constraint)))
        (equal 1 (position (name other) (scope constraint)))))
    (binary-constraints self)))

(defmethod make-node-consistent ((self varyable))
  (loop for constraint in (unary-constraints self) do
    (setf (domain self)
      (remove-if-not
        (lambda (val) (funcall (predicate constraint) val))
        (domain self)))))

(defmethod revise ((self varyable) (other varyable))
  (let* ((changed NIL))
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
(print (domain (first *varyables*)))
(print (domain (second *varyables*)))

(print (revise (second *varyables*) (first *varyables*)))
(print (domain (first *varyables*)))
(print (domain (second *varyables*)))

; (defclass CSP ()
;   (variables)
;   (assignment)
;   (domain)
;   (constraints))
