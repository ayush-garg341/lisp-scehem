(define fact (lambda (x)
	       (if (= x 1)
		   1
		   (* x (fact (- x 1)))
		   )
	       
	       )
  )


(define (make-point x y)
  (cons x y )
  )


(define (point-x point)
  (car point)
  )


(define (point-y point)
  (cdr point)
  )

(define p1 (make-point 1 2))

(define stretch-point
  (lambda (pt scale)
    (make-point (* scale (point-x pt))
		(* scale (point-y pt))
		)
    )
  )

(stretch-point p1 5)


(define 1to4 (list 1 2 3 4 5))



(define first car)
(define rest cdr)
(define adjoin cons)

;; create sequence of numbers from - to
;; '() stands for nil or empty list

(define (enumerate-interval from to)
  (if (> from to)
      '()
      (adjoin from (enumerate-interval (+ 1 from) to))
      )
  )


(lambda(x) (* x x))


;; generalising summation, higher order procedures,

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))
      )
  )


(define (sum-integers a b)
  (sum (lambda(x) x) a (lambda(x) (+ x 1)) b)
  )


;;  derivative function, HOP

(define deriv
  (lambda (f)
    (lambda (x) (/ (- (f (+ x epsilon)) (f x))
		   epsilon
		   ))
    )
  )

(define square (lambda (y) (* y y)))

(define epsilon 0.001)

((deriv square) 5)

;; transforming a list, HOP

(define (MAP proc lst)
  (if (null? lst)
      '()
      (cons (proc (car lst)) (MAP proc (cdr lst)))
      )
  )

(MAP (lambda(x) (* 2 x)) (LIST 1 2 3 4))


;; filter a list, HOP

(define (filter pred lst)
  (cond ((null? lst) '())
	((pred (car lst))
	 (cons (car lst) (filter pred (cdr lst)))
	 )
	(else (filter pred (cdr lst)) )
	)
  )

(filter even? (list 1 2 3 4 5 6 7 8))


;; 

(define make-counter
  (lambda(N)
    (lambda()
      (set! N (+ 1 N))
      N)
    )
  )

(define c1 (make-counter 0))

(define c2 (make-counter 10))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x
      )
    )
  )


;; make-point

(define make-point (lambda (x y) (list x y)))

(define  origin (make-point 0 0))


;; symbols

(quote AYUSHGARG)

;; add list

(define x 20)
(list + x 3)
(list '+ x 3)

;; procudre used in deriv calculation lect. 9 pdf.
;; cadr -> car of cdr
;; caddr -> cdr of cdr
(define simple-expr? (lambda (x) (not (pair? x))))

(define sum-expr? (lambda (e) (and (pair? e) (eq? (car e) '+))))

(define var? (lambda (e) (and (not (pair? e)) (symbol? e))))

(define make-sum (lambda (e1 e2) (list '+ e1 e2)))

(define addend (lambda (sum) (cadr sum)))

(define augend (lambda (sum) (caddr sum)))

(define deriv (lambda (expr var)
		(cond
		 ((number? expr) 0)
		 ((var? expr) (if (eq? expr var) 1 0))
		 ((sum-expr? expr)
		  (make-sum (deriv (addend expr) var)
			    (deriv (augend expr) var)
			    ))

		 ((prod-expr? expr)
		  <handle-prod-expr>)

		 (else (error "unknown expr type" expr))
		 )
		))




;; ADT (abstract data types) for sums, example of defensive and data directed programming.

;; type:  Exp, Exp -> SumExp

(define (make-sum addend augend)
  (list '+ addend augend)
  )

;; type: anytype -> boolean (defensive programming)
(define (sum-exp? e)
  (and (pair? e) (eq? (car e) '+))
  )

;; type: sumExp -> Exp
(define (sum-addend sum) (cadr sum))
(define (sum-augend sum) (caddr sum))

;; type: number | sumExp -> number

(define (eval-1 exp)
  (cond
   ((number? exp) exp)
   ((sum-exp? exp)
    (+ (eval-1 (sum-addend exp))
       (eval-1 (sum-augend exp))))
   (else (error "Unknown expr " exp))))


;; ADT for ranges (no tags)

;; type: number, number -> range
(define (make-range min max) (list min max))

;; type: range -> number
(define (range-min range) (car range))
(define (range-max range) (cadr range))

;; type: range, range -> range
(define (range-add r1 r2)
  (make-range (+ (range-min r1) (range-min r2))
	      (+ (range-max r1) (range-max r2))
	      )
  )


;; eval for numbers|ranges|sumExp -> numbers|ranges

(define (eval-2 exp)
  (cond
   ((number? exp) exp)
   ((sum-exp? exp)
    (let ((v1 (eval-2 (sum-addend exp)))
	  (v2 (eval-2 (sum-augend exp))))

      (if (and (number? v1) (number? v2))
	  (+ v1 v2)
	  (range-add v1 v2))))
   
   ((pair? exp) exp)
   (else (error "Unknown exp " exp))
   )
  )


;; =======================================================
;; ways in which eval-2 is broken

;; Missing a case: sum of number and a range
(eval-2 (make-sum 4 (make-range 4 6)))


;;=========================================================
;; Start again using tagged data

(define sum-tag '+)

;; type: exp, exp -> sumExp
(define (make-sum addend augend)
  (list sum-tag addend augend)
  )

;; type: anytype -> boolean
(define (sum-exp? exp)
  (and (pair? exp) (eq? (car exp) sum-tag))
  )


(define constant-tag 'const)

;; type: number -> constantExp
(define (make-const val) (list constant-tag val))

;; type: anyType -> boolean
(define (constant-exp? exp)
  (and (pair? exp) (eq? (car exp) constant-tag))
  )

;; type: constantExp -> number
(define (const-val const) (cadr const))

;; type: constantExp|sumExp -> constantExp
(define (eval-4 exp)
  (cond
   ((constant-exp? exp) exp)
   ((sum-exp? exp)
    (
     make-const
     (+ (const-val (eval-4 (sum-addend exp)))
	(const-val (eval-4 (sum-augend exp)))
	)
     )
    )
   (else (error "unknown type: " exp))
   )
  )


(eval-4 (make-sum (make-const 3) (make-const 4)))

;; type: constExp, constExp -> constExp

(define (const-add c1 c2)
  (make-const (+ (const-val c1) (const-val c2)))
  )


(define (eval-4 exp)
  (cond
   ((constant-exp? exp) exp)
   ((sum-exp? exp)
    (const-add
      (eval-4 (sum-addend exp))
       (eval-4 (sum-augend exp))

       )
    )
   (else (error "unknown type: " exp))
   )
  )

(eval-4 (make-sum (make-const 3) (make-const 4)))


;; when checking types use the else branch only for errors.


;; Alist operation : find assoc table adt - lec 11
;; caar : car of car
;; cadar : car of cdr of car
(define (find-assoc key alist)
  (
   cond
   ((null? alist) #f)
   ((equal? key (caar alist)) (cadar alist))
   (else (find-assoc key (cdr alist)))
   )
  )

(define al '((x 15) (y 20)))
(find-assoc 'y al)


(define (add-assoc  key value alist)
  (cons (list key value) alist)
  )

(define a2 (add-assoc 'y 10 al))
(find-assoc 'y a2)

;; table ADT implemented as an Alist
(define table1-tag 'table1)
(define (make-table1) (cons table1-tag '()))
(define (table1-get tbl key)
  (find-assoc key (cdr tbl))
  )

(define (table1-put! tbl key val)
  (set-cdr! tbl (add-assoc key val (cdr tbl)))
  )

;; set-cdr! is in-built operation, make the cdr part of tbl point to new list returned by add-assoc, 11-lect-webhand. 


;; table-1 example
(define ttl (make-table1))
(table1-put! ttl 'y 20)
(table1-put! ttl 'x 15)
(table1-get ttl 'y)

;; What is it that makes it an abstract data type ? Key thing is the isolation of the abstraction from its users. We can't use any list processors, like car, cdr, map or filter directly on the table.




;; example hash function
(define (hash-a-point point N)
  (modulo (+ (point-x point) (point-y point)) N)
  )

;; So a hash function basically chooses a bucket (out of a fixed set of buckets) into which to put an object or from which to extract an object.

;;(make-vector) => a vector with size locations, each initially contains value.
;;(vector-ref v index) => whatever is stored at that index of v.
;;(vector-set! v index val) => stores val at that index of v.


;;Table-2 ADT implemented as hash tables lec-11-webhand
(define t2-tag 'table2)
(define (make-table2 size hashfunc)
  (let ((buckets (make-vector size '())))
    (list t2-tag size hashfunc buckets)
    )
  )

(define (size-of tbl) (cadr tbl))
(define (hashfunc-of tbl) (caddr tbl))
(define (buckets-of tbl) (cadddr tbl))

(define (table2-get tbl key)
  (let ((index ((hashfunc-of tbl) key (size-of tbl))))
    (find-assoc key (vector-ref (buckets-of tbl) index))
    )
  )

(define (table2-put! tbl key value)
  (let ((index ((hashfunc-of tbl) key (size-of tbl)))
	(buckets (buckets-of tbl))
	)
    (vector-set! buckets index
		 (add-assoc key value
			    (vector-ref buckets index)
			    ))
    )
  )

(define tt2 (make-table2 4 hash-a-point))
(table2-put! tt2 (make-point 5 5) 20)
(table2-put! tt2 (make-point 5 7) 20)
(table2-get tt2 (make-point 5 5))


;; Mutation lec-12-webhand
(define a (list 1 2))
(define b a)
(set-car! a 10)

(define x (list 3 4))
(define y (list 1 2))
(set-car! x y)
(set-cdr! y (cdr x))



;; stack ADT as a list
(define (make-stack) '())
(define (empty-stack? stack) (null? stack))
(define (insert stack elt) (cons elt stack))
(define (delete stack)
  (if (empty-stack? stack)
      (error "Stack underflow - delete")
      (cdr stack)
      )
  )
(define (top stack)
  (if (empty-stack? stack)
      (error "Stack underflow - top")
      (car stack)
      )
  )

;; problem with above implementation
(define s (make-stack))
(insert s 'a)
s
(set! s (insert s 'b))
s

;; Alternate implementation of stack
(define (make-stack) (cons 'stack '()))
(define (stack? stack)
  (and (pair? stack) (eq? 'stack (car stack)))
  )
(define (empty-stack? stack)
  (if (not (stack? stack))
      (error "Object not a stack: " stack)
      (null? (cdr stack))
      )
  )

(define (insert! stack elt)
  (cond ((not (stack? stack))
      (error "Object not a stack: " stack))
      (else
       (set-cdr! stack (cons elt (cdr stack))) stack
       )
      )
  )

;; set-cdr! is a side effect and hence need to return stack explicitly

(define (delete! stack)
  (if (empty-stack? stack)
      (error "Stack underflow - delete")
      (set-cdr! stack (cddr stack))
      )
  stack
  )

(define (top stack)
  (if (empty-stack? stack)
      (error "Stack underflow - top")
      (cadr stack)))




;; Simple queue implementation ADT
(define (make-queue) '())
(define (empty-queue? q) (null? q))
(define (front-queue? q)
  (if (empty-queue? q)
      (error "front of empty queue: " q)
      (car q)
      )
  )

(define (delete-queue? q)
  (if (empty-queue? q)
      (error "delete of empty queue: " q)
      (cdr q)
      )
  )

(define (insert-queue q elt)
  (if (empty-queue? q)
      (cons elt '())
      (cons (car q) (insert-queue (cdr q) elt))
      )
  )

;; the above implementation findings
;; insert-queue:
;; Time: T(n) = O(n) => linear in time
;; Space: S(n) = O(n) => linear in space


;; Queue ADT with better insertion algos and design.

(define nil '())

(define (front-pointer q) (cadr q))
(define (rear-pointer q) (cddr q))
(define (set-front-pointer! q item)
  (set-car! (cdr q) item)
  )
(define (set-rear-pointer! q item)
  (set-cdr! (cdr q) item)
  )

(define (make-queue)
  (cons 'queue (cons nil nil))
  )

(define (queue? q)
  (and (pair? q) (eq? 'queue (car q)))
  )

(define (empty-queue? q)
  (if (not (queue? q))
      (error "Object not a queue: " q)
      (null? (front-pointer q))
      )
  )

(define (front-queue q)
  (if (empty-queue? q)
      (error "Front of empty queue: " q)
      (car (front-pointer q))
      )
  )

(define (insert-queue? q elt)
  (let ((new-pair (cons elt nil)))
    (cond
     ((empty-queue? q)
      (set-front-pointer! q new-pair)
      (set-rear-pointer! q new-pair)
      q
      )
     (else
      (set-cdr! (rear-pointer q) new-pair)
      (set-rear-pointer! q new-pair)
      q
      )
     )
    )
  )

(define (delete-queue! q)
  (cond
   ((empty-queue? q)
    (error "delete of empty queue: " q))

   (else
    (set-front-pointer! q (cdr (front-pointer q))))
   
   )
  )

;; ====================================================
;; lecture-13 webhand.
