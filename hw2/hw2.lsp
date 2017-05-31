;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; BFS(FRINGE): Argument is a tree FRINGE, can be either a single element or a list. 
; It returns the elements visited in BFS order.
(defun BFS (FRINGE)
	; We need a helper function to store the queue and the result
	; queue and result are always lists, never a single element.
	; High level idea: Each iteration is for one level of the tree. Everything on that level
    ; are put into a queue. Then if the head of the queue is a single element, add it to the
    ; result, otherwise it's the next level, and we dequeue it as a new subtree.
	(defun BFS_helper (tree queue result)
		(cond
			; Case: tree is empty
			((null tree) 
				(cond 
					; queue also empty, nothing is left, return the result
					((null queue) result)
					; queue is a list, we'll dequeue it and treat it as a subtree
					((listp queue)
						(cond 
							; If the car is a list, treat it as a subtree.
							((listp (car queue)) (BFS_helper (car queue) (cdr queue) result))
							; If the car is a single element, append it to the result.
							(t (BFS_helper '() (cdr queue) (append result (list (car queue))))
						)
					)
				)
			))
			; Case: tree is a list, append its element to the queue
			((listp tree) (BFS_helper (cdr tree) (append queue (list (car tree))) result))
			; Case: tree is a single element, add it to the queue
			(t (BFS_helper '() (append queue tree) result))
		)
	)
	(BFS_helper FRINGE '() '())
)


;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (and 
		(equal (car S) T) 
		(equal (cadr S) T) 
		(equal (caddr S) T) 
		(equal (cadddr S) T)
	)
)


; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
	(let
	(
		(homer-position (car S))
		(baby-position (cadr S))
		(dog-postion (caddr S))
		(poison-position (cadddr S))
	)
	(cond
		; Homer
		((equal A 'h) 
			(cond 
				; Apply this action would leave baby and poison on the same side, and unsupervised
				((equal baby-position poison-position) nil)
				; Apply this action would leave baby and dog on the same side, and unsupervised
				((equal baby-position dog-postion) nil)
				(t (list (list (not homer-position) baby-position dog-postion poison-position)))
			)
		)
		; Baby: Change both the homer and baby position
		((equal A 'b) 
			(cond 
				; Baby and homer not on the same side
				((not (equal homer-position baby-position)) nil)
				(t (list (list (not homer-position) (not baby-position) dog-postion poison-position)))
			)
		)
		; Dog: Change both the homer and dog position
		((equal A 'd) 
			(cond 
				; Dog and homer not on the same side
				((not (equal homer-position dog-postion)) nil)
				; Apply this action would leave baby and poison on the same side, and unsupervised
				((equal baby-position poison-position) nil)
				(t (list (list (not homer-position) baby-position (not dog-postion) poison-position)))
			)
		)
		; Poison: Change both the homer and poison position
		((equal A 'p) 
			(cond 
				; Poison and homer not on the same side
				((not (equal homer-position poison-position)) nil)
				; Apply this action would leave baby and dog on the same side, and unsupervised
				((equal baby-position dog-postion) nil)
				(t (list (list (not homer-position) baby-position dog-postion (not poison-position))))
			)
		)
	))
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
	(append 
		(NEXT-STATE S 'h)
		(NEXT-STATE S 'b)
		(NEXT-STATE S 'd)
		(NEXT-STATE S 'p)
	)
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
	(cond
		; Stack is empty, return nil 
		((null STATES) nil)
		; Stack is a list
		((listp STATES)
			; If S is equal to the first element, return T, otherwise examine 
			; the rest of the list
			(if 
				(equal (car STATES) S) 
				T
				(ON-PATH S (cdr STATES))
			)
		)
	)
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
	(cond 
		; No legal state
		((null (car STATES)) nil)
		; This state is the final state
		((FINAL-STATE (car STATES)) (append PATH (list (car STATES))))
		; Otherwise, do a DFS
		(t (or (DFS (car STATES) PATH) (MULT-DFS (cdr STATES) PATH)))
	)
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
	(cond
		; S already on search path
		((ON-PATH S PATH) nil)
		; S is goal state
		((FINAL-STATE S) (append path (list s)))
		; Not to goal yet, perform a DFS search
		(t (MULT-DFS (SUCC-FN S) (append path (list S))))
	)
)
