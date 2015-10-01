; Sorting method that returns the list in descending
; order. Done by returning the list where it takes the largest
; element of the list for the first value, then recursively
; sorts down the list where each value is "removed" so no
; duplicates are found.
(define (sortdesc L1)
	(cond
		((null? L1) '())
		(#t (cons (larger L1 (car L1)) (sortdesc (remove L1 (larger L1 (car L1))))))
	)
)

; This method will return an empty list if L1 is empty
; otherwise it will return L1 if L2 is empty because
; L1 does not need to be compared to any values. If both
; have values then if the first value of L1 is a member of
; L2 then it will "eliminate" recursively on the rest of the list.
; Finally if the first value is not a member it will return
; the list of both L1 and whatever values it "eliminates" in the
; rest of the list recursively.
(define (eliminate L1 L2)
	(cond
		((null? L1) '())
	 	((null? L2) L1)
		((xmemb (car L1) L2) (eliminate (cdr L1) L2))
		(#t (cons (car L1) (eliminate (cdr L1) L2)))
	)
)

; This method will return an empty list if either L1
; or L2 is empty because () && () is empty. Otherwise
; will check if the first value of L1 is in L2 and will
; return a list of the first value of L1 and whatever
; intersects with L2 in the rest of the list recursively.
; Otherwise will ignore the first value then return the
; intersection of the rest of L1 and L2.
(define (Ntersect L1 L2)
	(cond
    	((null? L1) '())
		((null? L2) '())
    	((xmemb (car L1) L2) (cons (car L1) (Ntersect (cdr L1) L2)))
    	(#t (Ntersect (cdr L1) L2)))
)

; Returns whether x is a member of the list.
(define (xmemb x List)
    (cond
		((null?  List)  #f)
		((eq?  x  (car  List)) #t)
        (#t (xmemb x (cdr List) ) )
	)
)


; "Removes" the largest element of the list.
(define (remove L1 x)
	(cond 
		((null? L1) '())
    	((eq?(car L1) x) (cdr L1)) 
    	(#t (cons (car L1) (remove (cdr L1) x)))
  	)
)

; Takes the largest element of the list.
(define (larger L1 x)
	(cond 
		((null? L1) x)
	    ((>(car L1) x) (larger (cdr L1) (car L1)))
	    (#t (larger (cdr L1) x))
	)
)

; eliminateNsort which is comprised of a sorting method
; a elimination method to "remove" values from the final
; list and the intersection method to get all values in
; both lists.
(define (eliminateNsort L1 L2)
	(sortdesc (eliminate L1 (Ntersect L1 L2)))
)
