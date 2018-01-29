;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname essential_pl-1.4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require racket/base)

(define TOO_SMALL_ERROR "The number is smaller than zero.")
(check-expect (duple 2 3) (list 3 3))
(check-expect (duple 4 (list "ha" "ha")) (list (list "ha" "ha")
                                               (list "ha" "ha")
                                               (list "ha" "ha")
                                               (list "ha" "ha")))
(check-expect (duple 0 (list "blah")) empty)
(check-expect (duple 3 empty) (list empty empty empty))
(check-error (duple -2 2) (string-append TOO_SMALL_ERROR " -2"))


;; duple: Natural X SchemeValue -> ListOfSymbol
;; produce a list of n copies of x
(define (duple n x)
  (if (< n 0)
      (error TOO_SMALL_ERROR n)
      (if (zero? n)
          empty
          (cons x
                (duple (- n 1) x)))))



;; invert: ListofDuple -> ListOfDuple
(check-expect (invert (list (list 1 2) (list 3 4))) (list (list 2 1) (list 4 3)))
(check-expect (invert empty) empty)
(check-expect (invert (list empty (list 5 6) (list (list 1 3) 0)))
              (list empty
                    (list 6 5)
                    (list 0 (list 1 3))))

(define (invert lst)
  (if (empty? lst)
      empty
      (cons (reverse (first lst))
            (invert (rest lst)))))

;; reverse: Duple -> Duple
;; where Duple ::= (list Symbol Symbol)

(check-expect (reverse (list 1 2)) (list 2 1))
(check-expect (reverse empty) empty)
(check-expect (reverse (list empty (list 1 2))) (list (list 1 2) empty))

(define (reverse duple)
  (if (empty? duple)
      empty
      (list (first (rest duple)) (first duple)))) 



;; down: this is not going to compile in Racket...


;; swapper: S-Exp X S-Exp X S-List
(check-expect (swapper 'a 'b empty) empty)
(check-expect (swapper 'a 'b (list 'a)) (list 'b))
(check-expect (swapper 'a 'd  (list 'a 'b 'c 'd))
              (list 'd 'b 'c 'a))
(check-expect (swapper 'a 'd  (list 'a 'b empty 'c 'd))
              (list 'd 'b empty 'c 'a))
;(check-expect (swapper empty 'd (list 'a 'b empty 'c 'd))
;              (list 'a 'b 'd 'c empty))  ;; possible but complicated to make work
(check-expect (swapper 'a 'd (list (list 'a) 'b (list (list 'c) empty) 'd))
              (list (list 'd) 'b (list (list 'c) empty) 'a))
; (check-expect (swapper invert reverse (list reverse)) (list invert)) ;; cannot compare
;; function equality with eqv?

(define (swapper s1 s2 slist)
  (if (empty? slist)
      empty
      (cons (replace-exp s1 s2 (first slist))
            (swapper s1 s2 (rest slist)))))

;; lack of type safety important for mathematical PL theory?
;; replace-exp: S-Exp X S-Exp X S-Exp
(check-expect (replace-exp 'a 'b 'a) 'b)
(check-expect (replace-exp 'a 'b 'b) 'a)
(check-expect (replace-exp 'a empty 'a) empty)
(check-expect (replace-exp 'a 'b (list 'a 'b)) (list 'b 'a))
(check-expect (replace-exp 'a 'b (list (list 'b 'b) 'a 'd))
              (list (list 'a 'a) 'b 'd))
(check-expect (replace-exp 'a 'a (list 'a 'a)) (list 'a 'a))
;(check-expect (replace-exp (list 'a) 'a (list 'a (list 'a)))
;              (list (list 'a) 'a)) ; !!! this produces a bizarre result - step through 

(define (replace-exp x y elt)
  (if (symbol? elt)
      (if (equal? elt x)
          y
          (if (eqv? elt y)
              x
              elt))
      (swapper x y elt)))

;; oh, whoops, only slist is s-list

;; list-set: List X Integer X SchemeValue -> List
;; replace nth element with x
(check-error (list-set empty 0 'a))
(check-error (list-set (list 'a 'b 'c) 3 'a))
(check-expect (list-set (list 'a 'b 'c) 1 'd) (list 'a 'd 'c))
(check-expect (list-set (list 'a 'b 'c) 2 3) (list 'a 'b 3))
(check-expect (list-set (list 'a 'b 'c) 0 'd) (list 'd 'b 'c))

;; classic me not doing the cross-product table - this empty? isn't needed
;; as if we get to a point where we didn't evaluate n = 0 but it's empty
;; then that automatically means too big
#;
(define (list-set lst n x)
  (if (empty? lst)
      (if (= -1 n)
          empty
          (error "The index is out-of-bounds"))
      (if (= 0 n)
          (cons x (rest lst))
          (cons (first lst) (list-set (rest lst) (- n 1) x)))))

(define (list-set lst n x)
  (if (empty? lst)
      (error "The index is out-of-bounds")
      (if (= 0 n)
          (cons x (rest lst))
          (cons (first lst) (list-set (rest lst) (- n 1) x)))))


;; count-occurrences: way too much of a classic 110 problem

(define-struct testresult (expected input1 input2))
;; product: ListofSymbol X ListofSymbol -> ListOf(ListOfSymbol)?
;; cartesian product
;; another classic cross-product tables one
(check-expect (product empty empty) empty)
(check-expect (product empty (list 1 2)) empty)


(check-expect (product (list 'a 'b 12 5 'c) (list '1 3))
              (list (list 'a 1) (list 'a 3) (list 'b 1) (list 'b 3)
                    (list 12 1) (list 12 3) (list 5 1) (list 5 3) (list 'c 1) (list 'c 3)))

#;
(check-satisfied (make-testresult
                  (list (list 'a '1) (list 'b '1) (list 12 '1) (list 5 '1) (list 'c '1)
                        (list 'a 3) (list 'b 3) (list 12 3) (list 5 3) (list 'c 3))
                  (list 'a 'b 12 5 'c) (list '1 3))
                 product-contains-all)


(define (product lst1 lst2)
  (if (empty? lst1)
      empty
      (append (expand (first lst1) lst2)
              (product (rest lst1) lst2))))

(check-expect (expand 'a (list 'a 'b)) (list (list 'a 'a) (list 'a 'b)))
(define (expand x lox)
  (if (empty? lox)
      empty
      (cons (list x (first lox))
            (expand x (rest lox)))))
      



;; for testing purposes but doesn't currently work...
#;
(check-expect (product-contains-all
               (make-testresult
                (list (list 'a 'b) (list 'a 1))
                (list 'a)
                (list 'b 1))) true)
#;
(define (product-contains-all tr)
  (andmap (λ (elt) (member elt
                           (product (testresult-input1 tr)
                                    (testresult-input2 tr))))
          (testresult-expected tr)))


;; filter-in: (SchemeValue -> Boolean) X ListOfSchemeValue
;; return elements satisfying pred
(check-expect (filter-in symbol? (list 'a 2 (list 1 3) 'b make-testresult))
              (list 'a 'b))
(check-expect (filter-in number? (list 1 3 (list 2 1) (make-testresult 1 2 3))) (list 1 3))

(define (filter-in pred? lst)
  ;  (filter pred lst))
  ; hm... that implementation would be too easier
  (if (empty? lst)
      empty
      (if (pred? (first lst))
          (cons (first lst) (filter-in pred? (rest lst)))
          (filter-in pred? (rest lst)))))


;; okay, I already implemented these built-ins when I took the course the first time
(check-expect (flatten (list 1 2 3)) (list 1 2 3))
(check-expect (flatten (list empty (list 1) (list 2 (list 3))))
              (list 1 2 3))
(define (flatten lst)
  (if (empty? lst)
      empty
      (if (list? (first lst))
          (append (flatten (first lst)) (flatten (rest lst)))
          (cons (first lst) (flatten (rest lst))))))


;;merge (sort)
(check-expect (merge (list 1 4) (list 1 2 8))
              (list 1 1 2 4 8))
(check-expect (merge (list 35 62 81 90 91) (list 3 83 85 90))
              (list 3 35 62 81 83 85 90 90 91))
(check-expect (merge empty (list 1 2)) (list 1 2))

(define (merge lst1 lst2)
  (if (or (empty? lst1) (empty? lst2))
      (append lst1 lst2)
      (if (<= (first lst1) (first lst2))
          (cons (first lst1) (merge (rest lst1) lst2))
          (cons (first lst2) (merge lst1 (rest lst2))))))

;; sorting we did already

;; path - takes an int n and bst that has n
;; return the left-right to show how to find n
;; if n is root, empty list

;; BST is one of
;; - false
;; - (make-bst Int BST BST)
(define-struct bst (node ltree rtree))

;; should probably follow the definition a bit more closely
(check-expect (path 4 (make-bst 4 false false)) empty)
(check-expect (path 4 false) false)
(check-expect (path 4 (make-bst 5 false false)) false)
(check-expect (path 4 (make-bst 5
                              (make-bst 3 false
                                        (make-bst 4 false false))
                              (make-bst 7
                                        (make-bst 6 false false)
                                        (make-bst 8 false false))))
                    (list "left" "right"))

(define (path n btree)
  (cond [(false? btree) false]
        [(= n (bst-node btree)) empty]
        [(< n (bst-node btree))
         (if (not (false? (path n (bst-ltree btree))))
             (cons "left" (path n (bst-ltree btree)))
             false)]
        [else
         (if (not (false? (path n (bst-rtree btree))))
             (cons "right" (path n (bst-rtree btree)))
             false)]))
;; this can definitely be simplified... hm but this is more efficient by taking advantage of bst        
      
      