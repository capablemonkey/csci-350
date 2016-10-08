(load "lib/searchengine.lisp")
(load "lib/missionaries-and-cannibals.lisp")

; here we go

(print (path (breadth-first-search *missionaries-and-cannibals*)))
; (print (path (depth-first-search *missionaries-and-cannibals*)))
; (print (depth-first-search-with-duplicate-node-detection *missionaries-and-cannibals*))

; (defparameter *default-depth-limit* 12)
; (print (depth-first-search-with-depth-limit *missionaries-and-cannibals*))
; (print (best-first-search *missionaries-and-cannibals*))

; (print (steepest-ascent-hill-climbing *missionaries-and-cannibals*))
; (print (optimal-heuristic-search *missionaries-and-cannibals*))
; (print (path (a-star-search *missionaries-and-cannibals*)))

(load "lib/farmer.lisp")

; (print (breadth-first-search *farmer*))
; (print (depth-first-search *farmer*))
; (print (depth-first-search-with-duplicate-node-detection *farmer*))
; (defparameter *default-depth-limit* 7)
; (print (depth-first-search-with-depth-limit *farmer*))

(load "lib/water-jug")

; (print (breadth-first-search *water-jug*))
; (print (depth-first-search *water-jug*))
; (print (depth-first-search-with-duplicate-node-detection *water-jug*))
; (defparameter *default-depth-limit* 6)
; (print (depth-first-search-with-depth-limit *water-jug*))