;; Q1
;; (byTwos n m) returns the list of every other integer starting with n up to m.
;; Base Case: if n > m, the result is the empty list.
;; Hypothesis: Assume (byTwos (+ n 2) m) returns the list of every other integer
;; from n+2 up to m.
;; Recursive step: (byTwos n m) returns (cons n (byTwos (+ n 2) m))
(define (byTwos n m)
  (cond (( > n m) '())
        (else
         (cons n (byTwos (+ n 2) m)))))


;; Q2
;; (compress L)
;; base case: if L is nil, return nil
;; hypothesis: assume (compress L0) works for L0 = car(L) or L0 = cdr(L)
;; recursive step: if (car L) is list , then (compress (car L)) (compress (cdr L))),
;;   else, (cons (car L) (compress (cdr L))))
(define (compress L)
  (cond ((null? L) '())
        ((list? (car L)) (append (compress (car L)) (compress (cdr L))))
        ( else (cons (car L) (compress (cdr L))))))

;;(compress '((((1))) (2 (3 (4 5)) (6 7 (8)) 9) 10 11)

;; Q3
;; (rev-all L)
;; helper function (myrev old new)
;; begin with old=L new=nil
;; end with old=nil new=result
;; base case: if old is nil, return new (result is ready
;; hypothesis: myrev with old = car(old) or cdr(old) works
;; recursive step: if first element of old is a list, then put the reversed car(old) into the front of new
;;   else, put car(old) in the front of new
;; then we define the rev-all with one argument using old=L new=nil
(define (myrev old new)
  (cond ((null? old) new)
        ((list? (car old)) (myrev (cdr old) (cons (myrev (car old) '()) new)))
        (else (myrev (cdr old) (cons (car old) new)))))

(define (rev-all L) (myrev L '()))

;;(rev-all '(1 2 (3 4) (5 6 (7 8) 9) 10))

;; Q4
;; (equalTo? x y)
;; base case: if one of x y is null, use eq
;;   else if x and y are both atom, use eq
;;   else if x and y are one list, one not list, false
;; hypothesis : (equalTo? (car x) (car y)) and (equalTo? (cdr x) (cdr y)) works
;; recursive step: if x and y are both nonempty lists, check if both car(x),car(y) and cdr(x),cdr(y) are equal
(define (equalTo? x y)
  (cond ((or (null? x) (null? y)) (eq? x y))
        ((and (not (list? x)) (not (list? y))) (eq? x y))
        ((not (and (list? x) (list? y))) #f)
        (else (and (equalTo? (car x) (car y)) (equalTo? (cdr x) (cdr y))))))

;;(equalTo? '((9) (1 2)) '((9) (1 2)))    


;; Q5
;; (equalFns? fn1 fn2 domain)
;; calculate fn1 and fn2 on domain and reuse equalTo or equal
(define (equalFns? fn1 fn2 domain)
  (equal? (map fn1 domain) (map fn2 domain)))

;;(equalFns? (lambda (L) (car L)) (lambda (L) (cadr L)) '(((2 3) (2 3)) ((4 5) (4 5))))

;; Q6
;; (same-vals fn1 fn2 domain)
;; base case: domain is empty, then return empty
;; hypothesis: (same-vals fn1 fn2 (cdr domain)) works
;; recursive step: if fn1(car domain)=fn2(car domain) then return (car domain)and(same-vals fn1 fn2 (cdr domain))
;;   else, only (same-vals fn1 fn2 (cdr domain))
(define (same-vals fn1 fn2 domain)
  (cond ((null? domain) '())
        (else (if (equal? (fn1 (car domain)) (fn2 (car domain)))
                  (cons (car domain) (same-vals fn1 fn2 (cdr domain)))
                  (same-vals fn1 fn2 (cdr domain))))))

;;(same-vals (lambda (x) x)
;;(lambda (y) (abs y)) ;; abs give the absolute value
;;'(666 -3 -2 -100 -1 0 1 2 3))

;; Q7
;; (split x L)
;; base case: L is empty return (()())
;; hypothesis: cdr(L) can be splitted by x
;; recursive step: first calculate (split x (cdr L)). If car(L)>x then add it to the front of second list, else add it to the first list
(define (split x L)
  (cond ((null? L) '(()()))
        (else (let*((LAFTER (split x (cdr L))) (leftL (car LAFTER)) (rightL (car(cdr LAFTER))))
               (if (> (car L) x) (list leftL (cons (car L) rightL)) (list (cons (car L) leftL) rightL))))))

;;(split 7 '(1 9 2 8 3 10 4 6 5))

;; Q8
;; (psort L)
;; base case: L empty, return ();
;; hypothesis: psort on substring works (must shorter than L)
;; recursive step: split cdr L using (car L) and concat (psort leftL) (car L) (psort rightL)
(define (psort L)
  (cond ((null? L) '())
        (else (let*((LAFTER (split (car L) (cdr L))) (leftL (car LAFTER)) (rightL (car(cdr LAFTER))))
                (append (psort leftL) (cons (car L) (psort rightL)))))))

;;(psort '(7 6 6 6 6))

;; Q9
;; (applyToList f)
(define (applyToList f)
  (lambda (L) (map f L)))


;; Q10
;; (newApplyToList f)
(define (newApplyToList f)
  (letrec ((g (lambda(L) (cond ((null? L) '())
                              (else (cons (f (car L)) (g (cdr L)))))))) g))

;;(define g (newApplyToList (lambda (x) (* x 2))))
;;(g '(1 2 3 4 5))          
                
                  
 
  
  


  

