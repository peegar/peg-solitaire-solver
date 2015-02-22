#lang racket
;; Peyman Gardideh

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Dimension is an Int
;; requires: 1 <= Dimension <= 9

;; A Peg [position] is an Int
;; requires: 11 <= Peg <= 99
;;           neither digit can be zero or greater than the
;;             Dimension (for the corresponding board)

;; A Board is a (list Dimension (listof Peg))
;; The list contains INVALID Peg positions

;; A State is a (listof Peg)
;; requires: list is non-emtpy
;;           each Peg is VALID for the corresponding board

;; A Solution is one of:
;; * 'any
;; * Peg


(define no-solution-text (list (list "No Solution Found")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the sample board from the assignment

(define sample (list 4 (list 41 42 43 44)))
#|
....
....
....
    
|#

(define sample/init (list 22 23))
#|
....
.OO.
....
    
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the traditional cross pattern with default init state cross/init
;; with some additional (easier) init states you can use

(define cross (list 7 (list 11 12 16 17 21 22 26 27 61 62 66 67 71 72 76 77)))
#|
  ...  
  ...  
.......
.......
.......
  ...  
  ...  
|#

(define cross/init (list 13 14 15 23 24 25 31 32 33 34 35 36 37 41 42 43
                         45 46 47 51 52 53 54 55 56 57 63 64 65 73 74 75))
#|
  OOO  
  OOO  
OOOOOOO
OOO.OOO
OOOOOOO
  OOO  
  OOO  
|#

(define cross/submarine (list 34 42 43 44 45 46))
#|
  ...  
  ...  
...O...
.OOOOO.
.......
  ...  
  ...  
|#

(define cross/greek (list 24 34 42 43 44 45 46 54 64))
#|
  ...  
  .O.  
...O...
.OOOOO.
...O...
  .O.  
  ...  
|#

(define cross/small-diamond (list 24 33 34 35 42 43 45 46 53 54 55 64))
#|
  ...  
  .O.  
..OOO..
.OO.OO.
..OOO..
  .O.  
  ...  
|#

(define cross/big-diamond (list 14 23 24 25 32 33 34 35 36 41 42 43
                                45 46 47 52 53 54 55 56 63 64 65 74))
#|
  .O.  
  OOO  
.OOOOO.
OOO.OOO
.OOOOO.
  OOO  
  .O.  
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (build-board dimentions) consumes dimentions and produces a 
;;    (listof (listof Peg)) corresponding to a list of dimentions rows, 
;;    with each row containing a list of dimentions Peg positions in that row.
;; build-board: Nat -> (listof (listof Peg))

(define (build-board dimentions)
  (build-list
   dimentions 
   (lambda (x) (build-list dimentions 
                           (lambda (y) (+ (+ 11 y) (* 10 x)))))))


;; (state->los board state) consumes a board and a state and produces a 
;;    (listof Str) corresponding to one string per row. Each character in the 
;;    string will correspond to a Peg position, with a #\space representing an 
;;    invalid position, an #\O (uppercase O, not zero) representing a position 
;;    occupied by a peg, and a #\. (a period) representing an unoccupied 
;;    position.
;; state->los: Board State -> (listof Str)

(define (state->los board state)
  (local [(define numbered-board (build-board (first board)))
          (define empty-board 
            (build-list (first board) (lambda (x) 
                                        (build-list (first board) 
                                                    (lambda (y) #\.)))))
          (define (replace char positions my-board)
            (map (lambda (board-row1 board-row2) 
                   (map (lambda (board-val1 board-val2)
                          (cond [(member? board-val2 positions) char]
                                [else board-val1]))
                        board-row1 board-row2))
                 my-board numbered-board))]
    (map list->string 
         (replace #\space (second board) 
                  (replace #\O state empty-board)))))


;; (make-solved? sol) consumes a sol and generates a new predicate function
;;    that consumes a State and produces true if the state is a solution
;; make-solved?: Solution -> (State -> Bool)

(define (make-solved? sol)
  (lambda (lst) (and (= (length lst) 1)
                     (or (equal? sol 'any)
                         (= (first lst) sol)))))


;; (neighbours board state) consumes a boeard and a state and produces
;;    a (listof State) which corresponds to every legal possible from 
;;    the original state
;; neighbours: Board State -> (listof State)

(define (neighbours board state)
  (local [(define dimention (first board))
          (define invalid-spots (second board))
          (define (insert val lst)
            (cond [(empty? lst) (list val)]
                  [(< val (first lst)) (cons val lst)]
                  [else (cons (first lst) 
                              (insert val (rest lst)))]))
          (define (taken? peg)
            (or (<= peg 10) (<= (remainder peg 10) 0) 
                (> (remainder peg 10) dimention)
                (> (quotient peg 10) dimention)
                (member? peg invalid-spots)
                (member? peg state)))
          
          (define (move-right peg)
            (cond [(and (member? (add1 peg) state)
                        (not (taken? (+ 2 peg))))
                   (insert (+ peg 2) 
                           (remove peg (remove (add1 peg) state)))]
                  [else empty]))
          
          (define (move-left peg)
            (cond [(and (member? (sub1 peg) state)
                        (not (taken? (- peg 2))))
                   (insert (- peg 2)
                           (remove peg (remove (sub1 peg) state)))]
                  [else empty]))
          
          (define (move-down peg)
            (cond [(and (member? (+ peg 10) state)
                        (not (taken? (+ 20 peg))))
                   (insert (+ peg 20)
                           (remove peg (remove (+ 10 peg) state)))]
                  [else empty]))
          
          (define (move-up peg)
            (cond [(and (member? (- peg 10) state)
                        (not (taken? (- peg 20))))
                   (insert (- peg 20)
                           (remove peg (remove (- peg 10) state)))]
                  [else empty]))
          
          (define (moves peg)
            (filter cons? (list (move-right peg)
                                (move-left peg)
                                (move-down peg)
                                (move-up peg))))]
    (foldr (lambda (x y) (append (moves x) y)) empty state)))


(define (make-neighbours board) (lambda (state) (neighbours board state)))


;; (find-route initial-state neighbours solved?) produces a sequence of
;;   states from initial-state to a solution, or false if no solution exists.
;;   solved? determines if a state is a solution.
;;   neighbours produces a list of legal next states from a given state.
;; find-route: X (X -> (listof X)) (X -> Bool) -> (anyof false (listof X))
(define (find-route initial-state neighbours solved?)
  (local
    [;; a NoRoute is a (make-noroute (listof Any))
     ;; a special structure to store unsuccessful visited states
     (define-struct noroute (visited))
     
     ;; (find-route/acc state visited) searches outward from state 
     ;;   looking for a path to a solution, accumulating visited states
     ;; find-route: X (listof X) -> (anyof NoRoute (listof X))
     (define (find-route/acc state visited)
       (cond [(solved? state) (list state)]
             [else 
              (local [(define route (find-route/list (neighbours state)
                                                     (cons state visited)))]
                (cond [(noroute? route) route]
                      [else (cons state route)]))]))
     
     ;; (find-route/list lostate visited) searches from every state in lostate
     ;;   looking for a path to a solution, accumulating visited states
     ;; find-route/list: (listof X) (listof X) -> (anyof NoRoute (listof X))     
     (define (find-route/list lostate visited)
       (cond [(empty? lostate) (make-noroute visited)]
             [(member (first lostate) visited)
              (find-route/list (rest lostate) visited)]
             [else
              (local [(define route (find-route/acc (first lostate) 
                                                    visited))]
                (cond [(noroute? route) 
                       (find-route/list (rest lostate)
                                        (noroute-visited route))]
                      [else route]))]))
     
     (define route (find-route/acc initial-state empty))]
    
    (cond [(noroute? route) false]
          [else route])))


;; (solitaire board state sol) consumes a board, a state, and a sol. 
;;    If a solution exists, solitaire produces a (listof State) that corresponds
;;    to a sequence of states starting with the the initial state and ending 
;;    with the solution state. Solitaire produces false if no solution exists.
;; solitaire: Board State Solution -> (Anyof (listof State) false))

(define (solitaire board state sol)
  (find-route state (make-neighbours board) (make-solved? sol)))


;; (result->text board result) consumes a Board and a result from solitaire and 
;;    produces a (listof (listof Str)) where each (listof Str) is the value 
;;    produced by state->los for the corresponding state in the sequence. 
;;    If no solution is found, this function produces the provided constant 
;;    no-solution-text.
;; result->text: Board (listof State) -> (listof (listof Str))

(define (result->text board result)
  (cond [(false? result) no-solution-text]
        [else (map (lambda (x) (state->los board x)) result)]))