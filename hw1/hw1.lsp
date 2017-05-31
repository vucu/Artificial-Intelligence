; PAD(n): Argument is a nonnegative integer. It returns the (n)th number of the PAD sequence
(defun PAD (n) 
    (cond
    ((= n 0) 1)				; 0 case
    ((= n 1) 1)				; 1 case
    ((= n 2) 1)				; 2 case
    (
        (> n 2) 			; >2 case, using the formula
        (+ (PAD (- n 2)) (PAD (- n 3)))
    )
))

; SUMS(n): Argument is a nonnegative integer. It returns the number of additions used by PAD(n)
(defun SUMS (n) 
    (cond
    ((= n 0) 0)				; 0 case: requires no addition
    ((= n 1) 0)				; 1 case
    ((= n 2) 0)				; 2 case
    (
		; >2 case, it's the number of addition of PAD(n-2), PAD(n-3), and one more addition
		; to add them together
        (> n 2) 			
        (+ (SUMS (- n 2)) (SUMS (- n 3)) 1)
    )
))

; ANON(TREE): Argument is a TREE, can be either a single element or a list. 
; It returns TREE with all elements replaced by '?
(defun ANON (TREE)
	(cond 
	; Case: TREE is empty, return nil
	((null TREE) nil)
	; Case: TREE is a list
	(
		(listp TREE) 
		(cond 
			; First element of TREE is a list, calling ANON on it,
			; and append it to the ANON of the rest
			((listp (car TREE)) (cons (ANON (car TREE)) (ANON (cdr TREE)))) 
			; First element of TREE is a single node, just convert it to '?, 
			; and append it to the ANON of the rest
			(t (cons '? (ANON (cdr TREE))))
		) 
	) 		    
	; Case: TREE is a single node
	(t '?)
	)
)