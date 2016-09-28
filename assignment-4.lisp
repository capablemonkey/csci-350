(DEFUN ALL-FUNS (LIST)
  (COND
    ((ENDP LIST) NIL)
    ((FIRST LIST) (CONS 'FUN (ALL-FUNS (REST LIST))))
    (T (CONS NIL (ALL-FUNS (REST LIST))))))

(PRINT (ALL-FUNS '(8 mom 3 pop 42 nil foo)))
(PRINT (ALL-FUNS '(8 (cake (7 nil (ice cream)) 0 (nil)) (((foo))))) )
(PRINT (ALL-FUNS '(1 2 3 NIL)))
(PRINT (ALL-FUNS '()))
(PRINT (ALL-FUNS (ALL-FUNS '(1 2 3 NIL))))

(DEFUN COUNTER (ATOM LIST)
  (COND
    ((ENDP LIST) 0)
    ((EQUAL ATOM (FIRST LIST)) (+ 1 (COUNTER ATOM (REST LIST))))
    (T (+ 0 (COUNTER ATOM (REST LIST))))))

(PRINT (COUNTER 3 '(1 2 3 3 3 4)))
(PRINT (COUNTER NIL '()))
(PRINT (COUNTER NIL '(1 2 3 () NIL (()))))
(PRINT (COUNTER '(NIL T) '(1 2 3 (()))))
(PRINT (COUNTER AAA AAA)

(DEFUN DOT-PRODUCT (A B)
  (REDUCE #'+ (MAPCAR #'* A B)))

(PRINT (DOT-PRODUCT '(1 2) '(3 4)))

(DEFUN CARTESIAN-PRODUCT (A B)
  (MAPCAN
    (LAMBDA (ITEM-FROM-A)
      (MAPCAR
        (LAMBDA (ITEM-FROM-B)
          (LIST ITEM-FROM-A ITEM-FROM-B))
        B))
    A))

(PRINT (CARTESIAN-PRODUCT '(A B C) '(1 2 3 4)))

(load "lib/searchengine.lisp")
(load "lib/water-jug-2.lisp")

(PRINT (BREADTH-FIRST-SEARCH *water-jug-2*))