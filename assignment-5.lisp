(load "lib/searchengine.lisp")

; define problem state

(defclass island-state (state)
  ((m-left
    :initarg :m-left
    :accessor m-left
    :documentation "missionaries on the left island")
  (c-left
    :initarg :c-left
    :accessor c-left
    :documentation "cannibals on the left island")
  (boat-is-left
    :initarg :boat-is-left
    :accessor boat-is-left
    :documentation "true if boat is at left island")))

(defmethod m-right ((self island-state))
  (- 3 (m-left self)))

(defmethod c-right ((self island-state))
  (- 3 (c-left self)))

(defmethod equal-states ((self island-state) (other island-state))
  (and
    (equal (m-left self) (m-left other))
    (equal (c-left self) (c-left other))))

(defmethod copy ((self island-state))
  (make-instance 'island-state :m-left (m-left self) :c-left (c-left self)))

(defmethod everyone-is-left ((self island-state))
  (and
    (= 3 (m-left self))
    (= 3 (c-left self))))

(defmethod estimated-distance-from-goal ((self island-state))
  ; TODO: do we need this?
  )

; operators

(defmethod is-valid-successor ((previous island-state) (new island-state))
  (cond
    ; ensure that the direction of travel is valid:
    ((equal (boat-is-left previous) (boat-is-left new)) NIL)
    ; ensure enough cannibals or missionaries to perform action
    ((> (c-left new) 3) NIL)
    ((> (m-left new) 3) NIL)
    ((< (c-left new) 0) NIL)
    ((< (m-left new) 0) NIL)
    ; check if missionaries will be eaten:
    ((and
      (> (c-left new) (m-left new))
      (> (m-left new) 0)) NIL)
    ((and
      (> (c-right new) (m-right new))
      (> (m-right new) 0)) NIL)
    (T T)))

(defmethod new-valid-successor-or-nil ((self island-state) operator)
  (let ((new (copy self)))
    (funcall operator new)
    (when (is-valid-successor self new) new)))

(defmethod move-2c-left ((self island-state))
  (new-valid-successor-or-nil self
    (lambda (new)
      (setf (c-left new) (+ 2 (c-left new)))
      (setf (boat-is-left new) T))))

(defmethod move-2c-right ((self island-state))
  (new-valid-successor-or-nil self
    (lambda (new)
      (setf (c-left new) (+ -2 (c-left new)))
      (setf (boat-is-left new) NIL))))

(defmethod move-2m-left ((self island-state))
  (new-valid-successor-or-nil self
    (lambda (new)
      (setf (m-left new) (+ 2 (m-left new)))
      (setf (boat-is-left new) T))))

(defmethod move-2m-right ((self island-state))
  (new-valid-successor-or-nil self
    (lambda (new)
      (setf (m-left new) (+ -2 (m-left new)))
      (setf (boat-is-left new) NIL))))

(defmethod move-1c-left ((self island-state))
  (new-valid-successor-or-nil self
    (lambda (new)
      (setf (c-left new) (+ 1 (c-left new)))
      (setf (boat-is-left new) T))))

(defmethod move-1c-right ((self island-state))
  (new-valid-successor-or-nil self
    (lambda (new)
      (setf (c-left new) (+ -1 (c-left new)))
      (setf (boat-is-left new) NIL))))

(defmethod move-1m-left ((self island-state))
  (new-valid-successor-or-nil self
    (lambda (new)
      (setf (m-left new) (+ 1 (m-left new)))
      (setf (boat-is-left new) T))))

(defmethod move-1m-right ((self island-state))
  (new-valid-successor-or-nil self
    (lambda (new)
      (setf (m-left new) (+ -1 (m-left new)))
      (setf (boat-is-left new) NIL))))

(defmethod move-1m1c-left ((self island-state))
  (new-valid-successor-or-nil self
    (lambda (new)
      (setf (m-left new) (+ 1 (m-left new)))
      (setf (c-left new) (+ 1 (c-left new)))
      (setf (boat-is-left new) T))))

(defmethod move-1m1c-right ((self island-state))
  (new-valid-successor-or-nil self
    (lambda (new)
      (setf (m-left new) (+ -1 (m-left new)))
      (setf (c-left new) (+ -1 (c-left new)))
      (setf (boat-is-left new) NIL))))

(defparameter *missionaries-and-cannibals*
  (make-instance 'problem
    :start-state (make-instance 'island-state :m-left 0 :c-left 0 :boat-is-left NIL)
    :goal-test 'everyone-is-left
    :operators '(move-2c-left move-2c-right move-1c-left move-1c-right move-2m-left move-2m-right move-1m-left move-1m-right move-1m1c-left move-1m1c-right)
    :name "missionaries and cannibals"))

; tests to ensure operators don't result in invalid states:

(if (equal NIL (move-2c-left (make-instance 'island-state :boat-is-left NIL :m-left 0 :c-left 0)))
  (print "fail") (print "pass"))

(if (equal NIL (move-2c-left (make-instance 'island-state :boat-is-left NIL :m-left 0 :c-left 1)))
  (print "fail") (print "pass"))

(if (equal NIL (move-2c-left (make-instance 'island-state :boat-is-left NIL :m-left 0 :c-left 2)))
  (print "pass") (print "fail"))

(if (equal NIL (move-2c-left (make-instance 'island-state :boat-is-left NIL :m-left 1 :c-left 0)))
  (print "pass") (print "fail"))

(if (equal NIL (move-2c-left (make-instance 'island-state :boat-is-left NIL :m-left 2 :c-left 0)))
  (print "fail") (print "pass"))

; here we go

(print (path (breadth-first-search *missionaries-and-cannibals*)))
