;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname exercise-2.5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
;; design an environment data structure using list-association representation


(define-struct env (vp env))
;; Environment is one of:
;; empty-environment (empty)
;; (make-environment ValuePair Environment)


(define-struct vp (var val))
;; ValuePair is (make-vp var val)

;; First function is empty-env
;; -> Environment
;; Produce the empty environment
(check-expect (empty-env) empty)
              
(define (empty-env)
  empty)


;; Identifier Value Environment -> Environment
;; add a new value to the environment
(check-expect (extend-env "Cat" 5 empty)
              (make-env (make-vp "Cat" 5) empty))

(check-expect (extend-env false "Dog"
                          (make-env (make-vp "Cat" 5) empty))
              (make-env (make-vp false "Dog")
                        (make-env (make-vp "Cat" 5) empty)))

(define (extend-env var val env)
  (make-env
   (make-vp var val)
   env))

;; Identifier Environment -> Value
;; Find the value associated with an identifier in environment

(check-expect(apply-env false (make-env (make-vp false "Dog")
                                        (make-env (make-vp "Cat" 5) empty))) "Dog")

(check-error (apply-env "" empty))

(check-expect (apply-env "Cat" (make-env (make-vp false "Dog")
                                         (make-env (make-vp "Cat" 5) empty))) 5)
(check-error (apply-env "Cabaret" (make-env (make-vp false "Dog")
                                         (make-env (make-vp "Cat" 5) empty))))

(define (apply-env var env)
  (if (empty? env)
                (error "There are no corresponding values.")
                (if (eqv? (vp-var (env-vp env)) var)
                    (vp-val (env-vp env))
                    (apply-env var (env-env env)))))


;; exercise 2.8
(check-expect (empty-env? empty) true)
(check-expect (empty-env? (make-env (make-vp false "Dog")
                                         (make-env (make-vp "Cat" 5) empty))) false)

(define (empty-env? env)
  (if (env? env)
      false
      (empty? env)))

(define (has-binding? var env)
  true)