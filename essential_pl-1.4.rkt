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
(check-expect (swapper empty 'd  (list 'a 'b empty 'c 'd))
              (list 'd 'b 'c 'a))
(check-expect (swapper 'a 'd (list (list 'a) 'b (list (list 'c) empty) 'd))
              (list (list 'd) 'b (list (list 'c) empty) 'a))
(check-expect (swapper invert reverse (list reverse)) (list invert))


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
(check-expect (replace-exp 'a 'b (list 'a 'b)) (list '