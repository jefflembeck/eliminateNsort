(define (xmemb x list)   ; is x a member of list?
    (cond
		( (null?  list)    #f)
		( (eq?  x  (car  list)) #t)   ; quote list?
        ( #t (xmemb x (cdr list) ) )
	)
)

(define (intersected L1 L2)
	(cond
    	((null? L1) '() )
    	((xmemb (car L1) L2) (cons (car L1) (intersected (cdr L1) L2)))
    	(#t (intersected (cdr L1) L2)))
)


(define (negated L1 L2)
	(cond
		((null? L1) '())
	 	((null? L2) L1)
		((xmemb (car L1) L2) (negated (cdr L1) L2)) 
		(#t (cons (car L1) (negated (cdr L1) L2)))
	)
)

(define (getmax L1 A)
	(cond ( (null? L1) A)
	      ( (> (car L1) A) (getmax (cdr L1) (car L1)))
	      (else (getmax (cdr L1) A ))
	)
)

;TODO
(define (sort L1)
	(cond
		((null? L1) '())
		((null? (cdr L1)) (car L1))
		(#t (cons (getmax L1 (car L1)) (sort (cdr L1))))
	)
)



(define (eliminateNsort L1 L2)
	(sort '(negated(L1 intersected(L1 L2))))
)