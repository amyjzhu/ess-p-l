;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname difftree) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DiffTree is one of:
;; - "one"
;; - (make-diff DiffTree DiffTree)
;; interp. a representation of an integer, where (diff dt1 dt2) = dt1 - dt2

(define-struct diff (tree1 tree2))
(define ZERO (make-diff "one" "one"))
(define TWO (make-diff "one" (make-diff (make-diff "one" "one")
                                        "one")))
(define NEG-ONE (make-diff (make-diff "one" "one") "one"))
(define THREE (make-diff TWO NEG-ONE))
 

#;
(define (fn-for-difftree dt)
  (cond [(and (string? dt)
              (string=? "one" dt)) ...]
        [else (... dt
                   (fn-for-diff (diff-tree1 dt))
                   (fn-for-diff (diff-tree2 dt)))]))


;; -> DiffTree
;; produces zero
(check-expect (zero) ZERO)

(define (zero)
  (make-diff "one" "one"))

;; DiffTree -> Boolean
;; produce true if a difftree is equal to zero
(check-expect (is-zero? (make-diff "one" "one")) true)
(check-expect (is-zero? "one") false)
(check-expect (is-zero? (make-diff "one" (make-diff "one" "one"))) false)
(check-expect (is-zero? (make-diff (make-diff "one" "one")
                                   (make-diff "one" "one"))) true)
(check-expect (is-zero? (make-diff (make-diff "one" "one")
                                   (make-diff "one" (make-diff "one" "one")))) false)
(define (is-zero? dt)
  (cond [(and (string? dt)
              (string=? "one" dt)) false]
        [else (=
               (diff-sum (diff-tree1 dt))
               (diff-sum (diff-tree2 dt)))]))


;; DiffTree -> Integer
;; produce the numeral reprsentation of dt
(check-expect (diff-sum (zero)) 0)
(check-expect (diff-sum "one") 1)
(check-expect (diff-sum (make-diff ZERO ZERO)) 0)
(check-expect (diff-sum NEG-ONE) -1)
(check-expect (diff-sum (make-diff (make-diff "one" "one")
                                       (make-diff (make-diff (make-diff (make-diff "one" "one")
                                                                        "one") "one") "one"))) 3)
(check-expect (diff-sum THREE) 3)

(define (diff-sum dt)
  (cond [(and (string? dt) (string=? "one" dt)) 1]
        [else (- (diff-sum (diff-tree1 dt))
                 (diff-sum (diff-tree2 dt)))]))


;; DiffTree -> DiffTree
;; produce the next largest integer represented as difftree
;; use diff-sum as the exact representation may vary
(check-expect (diff-sum (successor (zero))) 1)
(check-expect (diff-sum (successor TWO)) 3)
(check-expect (diff-sum (successor NEG-ONE)) 0)
(check-expect (diff-sum (successor (make-diff NEG-ONE "one"))) -1)
(check-expect (successor (make-diff "one" (make-diff "one" "one")))
              (make-diff 
               (make-diff "one" (make-diff "one" "one"))
               NEG-ONE))
(check-expect (successor (make-diff "one" (make-diff "one" "one")))
              (make-diff 
               (make-diff "one" (make-diff "one" "one"))
               (make-diff (make-diff "one" "one") "one")))
#; ; manual way
(define (successor dt)
  (make-diff dt
             (make-diff (make-diff "one" "one")
                        "one")))

(define (successor dt)
  (make-diff dt
             NEG-ONE))


;; DiffTree -> DiffTree
;; produce the next smaller integer represented as difftree

(check-expect (diff-sum (predecessor (zero))) -1)
(check-expect (diff-sum (predecessor TWO)) 1)
(check-expect (diff-sum (predecessor NEG-ONE)) -2)
(check-expect (diff-sum (predecessor THREE)) 2)
(check-expect (predecessor (make-diff "one" (make-diff "one" "one")))
              (make-diff (make-diff "one" (make-diff "one" "one"))
                         "one"))

(define (predecessor dt)
  (make-diff dt "one"))


;; Natural -> DiffTree
;; create a DiffTree representation of n
(check-expect (diff-sum (as-diff 0)) 0)
(check-expect (diff-sum (as-diff -7)) -7)
(check-expect (diff-sum (as-diff 19)) 19)

(define (as-diff n)
  (cond [(= 1 n) "one"]
        [(< 1 n) (successor (as-diff (- n 1)))]
        [else (predecessor (as-diff (+ n 1)))]))



;; DiffTree -> Boolean
;; produces true if shortest representation of difftree
(check-expect (shortest-diff "one") true)
(check-expect (shortest-diff ZERO) true)
(check-expect (shortest-diff (make-diff THREE (make-diff "one" "one"))) false)
(check-expect (shortest-diff (make-diff ZERO NEG-ONE)) false)

(define (shortest-diff dt)
  (local [(define (shortest-diff dt n)
            (if (>= n 1)
                (shortest-positive (diff-nodes dt) n)
                (shortest-negative (diff-nodes dt) n)))]
    (shortest-diff dt (diff-sum dt))))


;; DiffTree -> Natural
;; produces number of "one" difftrees in a difftree
(check-expect (diff-nodes ZERO) 2)
(check-expect (diff-nodes "one") 1)
(check-expect (diff-nodes NEG-ONE) 3)
(check-expect (diff-nodes (make-diff ZERO ZERO)) 4)

(define (diff-nodes dt)
  (fold-difftree 1 + dt))


;; Natural -> Boolean
;; produces true if number of nodes is shortest possible
;; should be
;; start with "one" and subtract -1 (i.e. (make-diff (make-diff "one" "one") "one")
;; for each after. thus, (- n 1) * 3 + 1

(check-expect (shortest-positive 1 1) true)
(check-expect (shortest-positive 4 2) true)
(check-expect (shortest-positive (+ 1 (* 5 3)) 6) true)
(check-expect (shortest-positive 6 2) false)

(define (shortest-positive nodes representation)
  (= (+ (* (- representation 1) 3) 1) nodes))

;; Natural -> Boolean
;; produces true if number of
(check-expect (shortest-negative 3 -1) true)
(check-expect (shortest-negative 4 -2) true)
(check-expect (shortest-negative 6 -2) false)
;; each time, just add (diff dt "one") from base negative one
              
(define (shortest-negative nodes representation)
  (= nodes (+ 3 (* (- -1 representation) 1))))



;; abstract fold
(define (fold-difftree b1 c1 dt)
  (cond [(and (string? dt)
              (string=? "one" dt)) b1]
        [else (c1 
                   (fold-difftree b1 c1 (diff-tree1 dt))
                   (fold-difftree b1 c1 (diff-tree2 dt)))]))

;; DiffTree DiffTree -> DiffTree
;; add
(check-expect (diff-sum (add ZERO ZERO)) 0)
(check-expect (diff-sum (add "one" ZERO)) 1)
(check-expect (diff-sum (add NEG-ONE NEG-ONE)) -2)
(check-expect (diff-sum (add NEG-ONE THREE)) 2)
(check-expect (add ZERO THREE)
              (make-diff ZERO
                         (make-diff (make-diff "one" "one")
                                    THREE)))

(define (add dt1 dt2)
  (make-diff dt1
             (make-diff ZERO
                        dt2)))
           


;; DiffTree -> DiffTree
;; produce the nth fibonnaci number as difftree
;; !!!