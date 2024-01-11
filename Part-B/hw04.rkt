
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Returns a list of numbers from low to high(including low and possibly high)
;; separated by stride and in sorted order
;; Assumption:
;;   low, high, stride are numbers, and stride > 0
(define (sequence low high stride)
  (cond [(> low high) '()]
        [(<= low high) (cons low (sequence (+ stride low)
                                           high
                                           stride))]))


;; Takes a list of strings xs and a string suffix and returns a list of
;; strings. Each element of the output should be the corresponding element
;; of the input appended with suffix.
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))


;; Takes a list xs and a number n.
;; If the number < 0, terminate the computation with (error "list-nth-mod: negative number")
;; Else if the list is empty, terminate the computation with (error "list-nth-mod: empty list")
;; Else return the i-th element of the list where we count from zero
;; and i is the remainder produced when dividing n by the list's length
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [else
         (let ([n (remainder n (length xs))])
           (car (list-tail xs n)))]))


;; Takes a stream s and a number n. It returns a list holding the first n values
;; produced by s in order
(define (stream-for-n-steps s n)
  (letrec ([helper (lambda (th i)
                     (if (>= i n)
                         '()
                         (cons (car th)
                               (helper ((cdr th)) (+ i 1)))))])
    (helper (s) 0)))
  

;; Write a stream that is like the stream of natural numbers (i.e., 1, 2, 3, ...)
;; except numbers divisable by 5 are negated
(define funny-number-stream
  (letrec ([f (lambda (i)
                (if (= 0 (remainder i 5))
                    (cons (* -1 i) (lambda () (f (+ i 1))))
                    (cons i (lambda () (f (+ i 1))))))])
    (lambda () (f 1))))


;; The elements of the stream alternate between the strings "dan.jpg" and "dog.jpg"
(define dan-then-dog
  (letrec ([f (lambda (s)
                (if (eq? s "dan.jpg")
                    (cons "dog.jpg" (lambda () (f "dan.jpg")))
                    (cons "dan.jpg" (lambda () (f "dog.jpg")))))])
    (lambda () (f "dog.jpg"))))


;; Takes a stream s and returns another stream.
;; If s would produce v for i-th element, then (stream-add-zero s) would produce
;; the pair (0 . v) for its i-th element
(define (stream-add-zero s)
  (letrec ([f (lambda (s)
                (cons (cons 0 (car (s)))
                      (lambda () (f (cdr (s))))))])
    (lambda () (f s))))


;; Takes two lists xs and ys and returns a stream
;; The lists may or may not be the same length, but assume they are both non-empty.
;; The elements produced by the stream are pairs where the first part is from xs
;; and the second part is from ys. The stream cycles forever through the lists
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n)
                            (list-nth-mod ys n))
                      (lambda () (f (+ 1 n)))))])
    (lambda () (f 0))))


;; Takes a value(v) and a vector(vec). It should behave like Racket's assoc library
;; function except
;; 1. It processes a vector instead of a list
;; 2. It allows vector elements not to be pairs in which case it skips them
;; 3. It always takes two arguments
(define (vector-assoc v vec)
  (letrec ([vec-length (vector-length vec)]
           [f (lambda (pos)
                (if (>= pos vec-length)
                    #f
                    (let ([element (vector-ref vec pos)])
                      (if (and (pair? element)
                               (= (car element) v))
                          element
                          (f (+ pos 1))))))])
    (f 0)))


;; Takes a list(xs) and a number(n) and returns a function that takes one argument v
;; and returns the same thing that (assoc v xs) would return.
;; The cache starts emtpy. When the function returned by cached-assoc is called
;; , it first checks the cache for the answer
(define (cached-assoc xs n)
  (let* ([cache (make-vector n #f)]
         [next_slot 0]
         [f (lambda (v)
              (let ([result (vector-assoc v cache)])
                (if result
                    result
                    (begin
                      (let ([tmp (assoc v xs)])
                        (if tmp
                            (begin
                              (vector-set! cache next_slot tmp)   ;; modify cache
                              (set! next_slot (remainder (add1 next_slot) n))
                              tmp)
                            #f))))))])
    f))
    
                                  

;; Define a macro that is used like (while-less e1 do e2) where e1 and e2
;; are expressions and while-less and do are syntax (keywords)
;; 1. It evaluates e1 exactly once.
;; 2. It evaluates e2 at least once.
;; 3. It keeps evaluating e2 until and only until the result is not a number
;;    less than the result of the evaluation of e1.
;; 4. Assuming evaluation terminates, the result is #t.
;; 5. Assume e1 and e2 produce numbers; your macro can do anything or fail
;;    mysteriously otherwise.
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([e1_val e1])
       (letrec ([loop (lambda (cur_val)
                        (if (>= cur_val e1_val)
                            #t
                            (begin (loop e2))))])
         (loop e2)))]))