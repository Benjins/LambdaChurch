#lang racket

;; Hello.  Welcome to the church of lambda.
;; Where it's always λ o'clock.

;; I couldn't find anything in default Racket after an exhaustive 4 minute search
;; So I just made this
(define test-count 0)

(define-syntax-rule (TEST x y)
  (if (equal? x y)
      (set! test-count (add1 test-count))
    (error (format "Test failed! Expected ~s got ~s" x y))))

(define true  (λ (a b) a))
(define false (λ (a b) b))

(define zero (λ (f) (λ (x) x)))
(define succ (λ (g) (λ (f) (λ (x) (f ((g f) x))))))

(define (num->LN n)
  (if (= n 0)
      zero
      (succ (num->LN (- n 1)))))

(define LN->num (λ (l) ((l add1) 0)))
(define LN-add (λ (a b) (λ (f) (λ (x) ((a f) ((b f) x))))))
(define LN-mul (λ (a b) (λ (f) (λ (x) ((a (b f)) x)))))
(define LN-exp (λ (a b) (λ (f) ((a b) f))))

(define LT-pair (λ (a b) (λ (x) (x a b))))
(define LT-first  (λ (p) (p true)))
(define LT-second (λ (p) (p false)))

(define LB-if (λ (cond t e) (cond t e)))

(TEST (LB-if true zero (succ zero)) zero)

(define LB-and (λ (a b) (LB-if a b false)))
(define LB-or  (λ (a b) (LB-if a true b)))
(define LB-not (λ (x)   (LB-if x false true)))
(define LB->bool (λ (x) (x #t #f)))
(define (bool->LB b) (if b true false))

(define or-not-self (λ (b) (LB-or b (LB-not b))))

(define LN-zero? (λ (ln) (LB-not ((ln or-not-self) false))))

(define LN-sub1_h (λ (t) (LB-if (LN-zero? (LT-second t))
                                (LT-pair zero (succ zero))
                                (LT-pair (succ (LT-first t)) (succ (LT-second t))))))

(define LN-sub1 (λ (n) (LT-first ((n LN-sub1_h) (LT-pair zero zero)))))

(define fact_b (λ (x r) ((LB-if (LN-zero? x)
                                (λ () (succ zero))
                                (λ () (LN-mul x
                                              (r (LN-sub1 x) r)))))))

(define fact (λ (x) (fact_b x fact_b)))

(TEST (LN->num (fact (num->LN 3))) 6)

(define LN-eq?_b (λ (a b r)
                   (LB-if (LN-zero? a)
                          (LN-zero? b)
                          ((LB-if (LN-zero? b)
                                  (λ () false)
                                  (λ () (r (LN-sub1 a) (LN-sub1 b) r)))))))

(define LN-eq? (λ (a b) (LN-eq?_b a b LN-eq?_b)))

(TEST ((LN-eq? (succ zero) (succ zero)) #t #f) #t)

(define LL-head LT-first)
(define LL-tail LT-second)
(define LL-cons LT-pair)

(define LL-nil (λ (x) true))
(define LL-null? (λ (p) (p (λ (x y) false))))

(define LL-length_b (λ (l r) ((LB-if (LL-null? l)
                                     (λ () zero)
                                     (λ () (succ (r (LL-tail l) r)))))))

(define LL-length (λ (l) (LL-length_b l LL-length_b)))

(define LL-sum_b (λ (l r)
                   ((LB-if (LL-null? l)
                           (λ () zero)
                           (λ () (LN-add (LL-head l)
                                         (r (LL-tail l) r)))))))

(define LL-sum (λ (l) (LL-sum_b l LL-sum_b)))

(define (numlist->LL l)
  (if (null? l)
      LL-nil
      (LL-cons (num->LN (first l))
               (numlist->LL (rest l)))))

(define (LL->numlist l)
  ((LB-if (LL-null? l)
          (λ () null)
          (λ () (cons (LN->num (LL-head l))
                      (LL->numlist (LL-tail l)))))))

(define LL-append_b (λ (a b r)
                      ((LB-if (LL-null? a)
                              (λ () b)
                              (λ () (LL-cons (LL-head a)
                                             (r (LL-tail a) b r)))))))

(define LL-append (λ (a b) (LL-append_b a b LL-append_b)))

(TEST (LN->num (LL-sum (numlist->LL '(1 5 4)))) 10)

(define (string->LS s)
  (numlist->LL (map char->integer (string->list s))))

(define (LS->string ls)
  ((LB-if (LL-null? ls)
          (λ () "")
          (λ () (string-append (string (integer->char (LN->num (LL-head ls))))
                               (LS->string (LL-tail ls)))))))

(define LS-append LL-append)

(TEST (LS->string (LS-append (string->LS "abc") (string->LS "HGF"))) "abcHGF")

;; So we know how many tests ran
(display (format "~s tests passed!" test-count))