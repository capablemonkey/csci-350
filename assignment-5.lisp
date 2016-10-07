(load "lib/searchengine.lisp")

; define search state

(defclass island-state (state)
  ((m-left
    :initarg :m-left
    :accessor m-left
    :documentation "missionaries on the left island")
  (c-left
    :initarg :c-left
    :accessor c-left
    :documentation "cannibals on the left island")))

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

(defmethod all-over ((self island-state))
  (let ((new (copy self)))
    (setf (m-left new) 3)
    (setf (c-left new) 3)
    new))

; operators:
; move-2c-left
; move-2c-right
; move-1c-left
; move-1c-right
; move-2m-left
; move-2m-right
; move-1m-left
; move-1m-right
; move-1m1c-left
; move-1m1c-right

; MM CCCM
; m: 1. c: 0

(defmethod is-valid-state ((self island-state))
  (cond
    ; check if action was not supposed to be performed
    ((> (c-left self) 3) NIL)
    ((> (m-left self) 3) NIL)
    ((< (c-left self) 0) NIL)
    ((< (m-left self) 0) NIL)
    ; check if missionaries will be eaten:
    ((and
      (> (c-left self) (m-left self))
      (> (m-left self) 0)) NIL)
    ((and
      (> (c-right self) (m-right self))
      (> (m-right self) 0)) NIL)
    (T T)))

(defmethod move-2c-left ((self island-state))
  (let ((new-state (copy self)))
    (setf (c-left new-state) (+ 2 (c-left new-state)))
    (describe new-state)
    (when (is-valid-state new-state) new-state)))

(defparameter *missionaries-and-cannibals*
  (make-instance 'problem
    :start-state (make-instance 'island-state :m-left 0 :c-left 0)
    :goal-test 'everyone-is-left
    :operators '()
    :name "missionaries and cannibals"))

; (describe (all-over (start-state *missionaries-and-cannibals*)))

(describe (move-2c-left (make-instance 'island-state :m-left 0 :c-left 0)))

; TODO: write tests to ensure operators don't result in invalid states:

(if (equal NIL (move-2c-left (make-instance 'island-state :m-left 0 :c-left 0)))
  (print "fail") (print "pass"))

(if (equal NIL (move-2c-left (make-instance 'island-state :m-left 0 :c-left 1)))
  (print "fail") (print "pass"))

(if (equal NIL (move-2c-left (make-instance 'island-state :m-left 0 :c-left 2)))
  (print "pass") (print "fail"))

(if (equal NIL (move-2c-left (make-instance 'island-state :m-left 1 :c-left 0)))
  (print "pass") (print "fail"))

(if (equal NIL (move-2c-left (make-instance 'island-state :m-left 2 :c-left 0)))
  (print "fail") (print "pass"))

; (describe (BREADTH-FIRST-SEARCH *missionaries-and-cannibals*))
