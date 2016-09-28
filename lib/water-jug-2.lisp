(defclass jug-state (state)
    ((four :initarg :four :initform nil :accessor four :documentation "amount in 4-gallon jug")
     (three :initarg :three :initform nil :accessor three :documentation "amount in 3-gallon jug")))

(defparameter *water-jug-2*
  (make-instance 'problem
    :start-state (make-instance 'jug-state :four 4 :three 3)
    :goal-test 'two-in-four
    :operators '(fill-4 fill-3 dump-4 dump-3 fill-4-from-3 fill-3-from-4 empty-3-into-4 empty-4-into-3)
    :name "water jug"))

;;;****************************************************************************************
;;;SEARCH SUPPORT FUNCTIONS
;;;****************************************************************************************

(defmethod equal-states ((self jug-state) (other jug-state))
  (and (equal (four self) (four other))
       (equal (three self) (three other))))

(defmethod copy ((self jug-state))
  (make-instance 'jug-state :four (four self) :three (three self)))

(defmethod two-in-four ((self jug-state))
    (= 2 (four self)))

(defmethod estimated-distance-from-goal ((self jug-state))
  (declare (ignore self))
  1)

;;;****************************************************************************************
;;;OPERATORS AND THEIR SUPPORTING DEFINITIONS
;;;****************************************************************************************

(defmethod fill-4 ((self jug-state)) 
  (when (< (four self) 4)
    (let ((copy (copy self)))
      (setf (four copy) 4)
      copy)))

(defmethod fill-3 ((self jug-state)) 
  (when (< (three self) 3) 
    (let ((copy (copy self)))
      (setf (three copy) 3)
      copy)))

(defmethod dump-4 ((self jug-state))
  (when (> (four self) 0)
    (let ((copy (copy self)))
      (setf (four copy) 0)
      copy)))

(defmethod dump-3 ((self jug-state))
  (when (> (three self) 0)
    (let ((copy (copy self)))
      (setf (three copy) 0)
      copy)))

(defmethod fill-4-from-3 ((self jug-state))
  (let ((old-3 (three self))
        (old-4 (four self))
        (copy (copy self)))
    (when (and (> old-3 0)
               (< old-4 4)
               (> (+ old-4 old-3) 4)) ;???>=
      (setf (three copy) (- old-3 (- 4 old-4)))
      (setf (four copy) 4) 
      copy)))

(defmethod fill-3-from-4 ((self jug-state))
    (let ((old-3 (three self))
          (old-4 (four self))
          (copy (copy self)))
      (when (and (> old-4 0)
                 (< old-3 4)) ;???really
        (setf (four copy) (- old-4 (- 3 old-3)))
        (setf (three copy) 3)
  copy)))

(defmethod empty-4-into-3 ((self jug-state))
  (let ((old-3 (three self))
        (old-4 (four self))
        (copy (copy self)))
    (when (and (> old-4 0)
               (<= (+ old-4 old-3) 3))
      (setf (three copy) (+ old-3 old-4))
      (setf (four copy) 0)
      copy)))

(defmethod empty-3-into-4 ((self jug-state))
  (let ((old-3 (three self))
        (old-4 (four self))
        (copy (copy self)))
    (when (and (> old-3 0)
               (<= (+ old-3 old-4) 4))
      (setf (four copy) (+ old-3 old-4))
      (setf (three copy) 0)
      copy)))