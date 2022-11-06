A(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponent? exp)
         (cond ((eq? (base exp) var)
		(make-product (exponent exp) (make-exponent (base exp) (- (exponent exp) 1))))
	       (else (list '* (exponent exp) (deriv (base exp) var) (make-exponent (base exp) (- (exponent exp) 1))))))
	(else
	 (error "unknown expression type -- DERIV" exp))))

(deriv '(** (+ (* 2 x) 3) 4) 'x)



(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))



(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum v1 v2)
  (cond ((=number? v1 0) v2)
	((=number? v2 0) v1)
	((and (number? v1) (number? v2)) (+ v1 v2))
	(else (list '+ v1 v2))))


(define (make-product v1 v2)
  (cond ((or (=number? v1 0) (=number? v2 0)) 0)
	((=number? v1 1) v2)
	((=number? v2 1) v1)
	((and (number? v1) (number? v2)) (* v1 v2))
	(else (list '* v1 v2))))



(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend x) (cadr x))

(define (augend x) (caddr x))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier x)
  (cadr x))

(define (multiplicand x) (caddr x))


(define (** base exponent)
  (cond ((= exponent 0) 1)
	((= exponent 1) base)
	(else (* base (** base (- exponent 1))))))

(define (exponent? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

(define (make-exponent b e)
  (cond ((or (=number? e 0) (=number? b 1)) 1)
	(else (list '** b e))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) z) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(define w '(** x 5))
(deriv w 'x)

(restart 1)
