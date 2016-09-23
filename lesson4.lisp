(DEFUN CONTAINS-NUMBER-P (LIST)
  (COND ((ENDP LIST) NIL)
    ((NUMBERP (FIRST LIST)) T)
    (T (CONTAINS-NUMBER-P (REST LIST)))))

(DEFUN SIMPLE-OR (LIST)
  (COND
    ((ENDP LIST) NIL)
    ((AND T (FIRST LIST)) T)
    ((NOT (FIRST LIST))
      (SIMPLE-OR (REST LIST)))))

(DEFUN X-IN-LIST-P (LIST)
  "does stuff"
  (COND
    ((ENDP LIST) NIL)
    ((EQUAL 'X (FIRST LIST)) T)
    (T (X-IN-LIST-P (REST LIST)))))

; (PRINT (X-IN-LIST-P '(1 2 3)))
; (PRINT (X-IN-LIST-P '(1 X 3)))

(DEFUN ALL-SAME-AS-ELEMENT (X LIST)
  (COND
    ((ENDP LIST)
      T)
    ((NOT (EQL X (FIRST LIST)))
      NIL)
    (T
      (ALL-SAME-AS-ELEMENT X (REST LIST)))))

; (PRINT (ALL-SAME-AS-ELEMENT 'U '(U U U)))
; (PRINT (ALL-SAME-AS-ELEMENT 'U '(U A U)))
; (PRINT (ALL-SAME-AS-ELEMENT 'U '()))

(DEFUN MONOTONIC-INCREASING-P (LIST)
  (COND
    ((NOT (SECOND LIST)) T)
    ((> (FIRST LIST) (SECOND LIST)) NIL)
    (T (MONOTONIC-INCREASING-P (REST LIST)))))

(PRINT (MONOTONIC-INCREASING-P '(1 2 3)))
(PRINT (MONOTONIC-INCREASING-P '(1 2 3 1 5)))