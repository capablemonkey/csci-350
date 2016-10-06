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

(defparameter *missionaries-and-cannibals*
  (make-instance 'problem
    :start-state (make-instance 'island-state :m-left 0 :c-left 0)
    :goal-test 'everyone-is-left
    :operators '(all-over)
    :name "missionaries and cannibals"))

(describe (all-over (start-state *missionaries-and-cannibals*)))

(describe (BREADTH-FIRST-SEARCH *missionaries-and-cannibals*))
