;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (and (not (isNil v)) (= v blank))
)

(defun isWall (v)
  (and (not (isNil v)) (= v wall))
)

(defun isBox (v)
	(and (not (isNil v)) (= v box))
)

(defun isKeeper (v)
	(and (not (isNil v)) (= v keeper))
)

(defun isStar (v)
	(and (not (isNil v)) (= v star))
)

(defun isBoxStar (v)
	(and (not (isNil v)) (= v boxstar))
)

(defun isKeeperStar (v)
	(and (not (isNil v)) (= v keeperstar))
)

; CUSTOM
(defun isNil (v)
	(null v)
)

(defun isObstacle (v)
	(or (isBox v) (isBoxStar v) (isWall v))
)

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

; CUSTOM
; getKeeperPosition-rc (s firstRow)
; Same as getKeeperPosition, but it return the position in (r c)
; format.
(defun getKeeperPosition-rc (s row)
	(let 
		((pos (getKeeperPosition s row)))
		(list (cadr pos) (car pos))
))

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
; box-not-exist: Return non-nil if box does not exist in a list, nil otherwise
(defun box-not-exist (lst)
	(or 
		(null lst) 
		(and (not (isBox (car lst))) (box-not-exist (cdr lst)))
	)
)

; box-not-exist-state: Return non-nil if box does not exist in the game state, nil otherwise
(defun box-not-exist-state (s)
	(or 
		(null s) 
		(and (box-not-exist (car s)) (box-not-exist-state (cdr s)))
	)
)
  
(defun goal-test (s)
	(box-not-exist-state s)
);end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.

; get-square: state s, row r, column c. Return the object in that square
(defun get-square (s r c)
	(cond 
		((< r 0) nil)
		((< c 0) nil)
		(t (car (nthcdr c (car (nthcdr r s)))))
	)
)

; Same as get-square, but take state s and the position tuple (r c) as parameters
(defun get-square-p (s p)
	(get-square s (first p) (second p))
)

; set-square: state s, row r, column c, object o. Return a new state s, with 
; position (r,c) set to object o.
(defun set-square (s r c o)
	(let 
		((h (length s))
		(w (length (car s))))
	(cond
		((< r 0) s)
		((>= r h) s)
		((< c 0) s)
		((>= c w) s)
		(t 
			(let*
				((all-rows-above (butlast s (- h r)))
				(all-rows-below (nthcdr (+ r 1) s))
				(current-row (car (nthcdr r s)))
				(all-cells-before (butlast current-row (- w c)))
				(all-cells-after (nthcdr (+ c 1) current-row))
				(new-row (append all-cells-before (list o) all-cells-after)))
			(append all-rows-above (list new-row) all-rows-below)
		))
	)
))

; Same as set-square, but take state s and the position tuple (r c) as parameters
(defun set-square-p (s p o)
	(set-square s (first p) (second p) o)
)

; The important positions are: The keeper, and two squares ahead of him, in direction.
; Return the tuples (r c) for positions.
(defun get-three-important-positions (s direction)
	(let* 
		((keeper-position (getKeeperPosition-rc s 0))
		(keeper-position-r (first keeper-position))
		(keeper-position-c (second keeper-position)))
	(cond 
		((equal direction 'up) 
			(list 
				keeper-position 
				(list (- keeper-position-r 1) keeper-position-c)
				(list (- keeper-position-r 2) keeper-position-c)
			)
		)
		((equal direction 'down) 
			(list 
				keeper-position 
				(list (+ keeper-position-r 1) keeper-position-c)
				(list (+ keeper-position-r 2) keeper-position-c)
			)
		)
		((equal direction 'left) 
			(list 
				keeper-position 
				(list keeper-position-r (- keeper-position-c 1))
				(list keeper-position-r (- keeper-position-c 2))
			)
		)
		((equal direction 'right) 
			(list 
				keeper-position 
				(list keeper-position-r (+ keeper-position-c 1))
				(list keeper-position-r (+ keeper-position-c 2))
			)
		)
	)
))

; Is the keeper pushing in the given direction
(defun is-pushing (s direction)
	(let* 
		((three-positions (get-three-important-positions s direction))
		(object1 (get-square-p s (second three-positions)))
		(object2 (get-square-p s (third three-positions))))
	(cond
		; Check if the object ahead is a box
		((not (or (isBox object1) (isBoxStar object1))) nil)
		; Check if he can push it
		((isNil object2) nil)
		((isWall object2) nil)
		((isBox object2) nil)
		((isBoxStar object2) nil)
		(t t)
	)
))

; Is the keeper merely moving in the given direction
(defun is-moving (s direction)
	(let* 
		((three-positions (get-three-important-positions s direction))
		(object1 (get-square-p s (second three-positions))))
	(cond
		; Check if the object ahead is a not a box. 
		((isBox object1) nil)
		((isBoxStar object1) nil)
		; Check for obstacles ahead
		((isNil object1) nil)
		((isWall object1) nil)
		(t t)
	)
))

; Return the square after removing the keeper.
(defun remove-keeper-p (s p)
	(let* 
		((the-keeper (get-square-p s p)))
	(cond
		((isKeeper the-keeper) blank)
		((isKeeperStar the-keeper) star)
	)
))
(defun add-keeper-p (s p)
	(let* 
		((the-object (get-square-p s p)))
	(cond
		((isBlank the-object) keeper)
		((isStar the-object) keeperstar)
	)
))
(defun remove-box-p (s p)
	(let* 
		((the-box (get-square-p s p)))
	(cond
		((isBox the-box) blank)
		((isBoxStar the-box) star)
	)
))
(defun add-box-p (s p)
	(let* 
		((the-object (get-square-p s p)))
	(cond
		((isBlank the-object) box)
		((isStar the-object) boxstar)
	)
))

; Move in the given direction
; Return the new state
(defun move (s direction)
	(let* 
		((three-positions (get-three-important-positions s direction))
		(the-keeper (get-square-p s (first three-positions)))
		(position-keeper (first three-positions))
		(object1 (get-square-p s (second three-positions)))
		(position1 (second three-positions))
		(object2 (get-square-p s (third three-positions)))
		(position2 (third three-positions)))
	(cond
		; Pushing case
		((is-pushing s direction) 
			(let*
				((s1 (set-square-p s position2 (add-box-p s position2)))
				(s2 (set-square-p s1 position1 (remove-box-p s1 position1)))
				(s3 (set-square-p s2 position1 (add-keeper-p s2 position1)))
				(s4 (set-square-p s3 position-keeper (remove-keeper-p s3 position-keeper))))
			s4
		))
		; Moving case
		((is-moving s direction) 
			(let*
				((s1 (set-square-p s position1 (add-keeper-p s position1)))
				(s2 (set-square-p s1 position-keeper (remove-keeper-p s1 position-keeper))))
			s2
		))
		; Else, we can't do anything, return nil
		(t nil)
	)
))

(defun next-states (s)
	(let* 
		((move-up (move s 'up))
		(move-down (move s 'down))
		(move-left (move s 'left))
		(move-right (move s 'right))
		(result (list move-up move-down move-left move-right)))
    (cleanUpList result)
))

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; Count the number of boxes in the state.
; Using an accumulator to get the tail-recursive
(defun count-box (s accum)
	(cond 
		((null s) accum)
		(t (count-box (cdr s) (+ accum (count box (car s)))))
	)
)

; Yes, it is admissable. Because for every misplaced box, we 
; need at least one move to move it to the goal (if the keeper
; is right next to it, and the goal is in the same direction).
; Thus the number of moves required will always be equal or 
; greater than the number of misplaced box.
(defun h1 (s)
	(count-box s 0)
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
(defun h504669405 (s)
	(let*
		((hA (h-assign s))
		(hB (h-deadlock s))
		(h-total (+ hA hB)))
	h-total
))

; Deadlock heuristics 

; Is there a deadlock at position (r, c)
; This function will recognize a deadlock by examine the 2x2 area.
(defun is-deadlock-at-position (s r c) 
	(let*
		((thing00 (get-square s r c))
		(thing01 (get-square s r (+ c 1)))
		(thing10 (get-square s (+ r 1) c))
		(thing11 (get-square s (+ r 1) (+ c 1))))
	(cond
		; ?#
		; #$
		((and (isWall thing01) (isWall thing10) (isBox thing11)) t)
		((and (isWall thing01) (isWall thing10) (isBox thing00)) t)
		((and (isWall thing00) (isWall thing11) (isBox thing01)) t)
		((and (isWall thing00) (isWall thing11) (isBox thing10)) t)
		; X denotes obstacles, be it another box, boxstar, or wall
		; #X
		; #$
		((and (isWall thing00) (isWall thing10) (isObstacle thing01) (isBox thing11)) t)
		((and (isWall thing00) (isWall thing01) (isObstacle thing10) (isBox thing11)) t)
		((and (isWall thing10) (isWall thing11) (isObstacle thing00) (isBox thing01)) t)
		((and (isWall thing01) (isWall thing11) (isObstacle thing00) (isBox thing10)) t)
		((and (isWall thing00) (isWall thing10) (isBox thing01) (isObstacle thing11)) t)
		((and (isWall thing00) (isWall thing01) (isBox thing10) (isObstacle thing11)) t)
		((and (isWall thing10) (isWall thing11) (isBox thing00) (isObstacle thing01)) t)
		((and (isWall thing01) (isWall thing11) (isBox thing00) (isObstacle thing10)) t)
		; XX
		; X$
		((and (isBox thing00) (isObstacle thing01) (isObstacle thing10) (isObstacle thing11)) t)
		((and (isObstacle thing00) (isBox thing01) (isObstacle thing10) (isObstacle thing11)) t)
		((and (isObstacle thing00) (isObstacle thing01) (isBox thing10) (isObstacle thing11)) t)
		((and (isObstacle thing00) (isObstacle thing01) (isObstacle thing10) (isBox thing11)) t)
		(t nil)
	)
))

; Is the state s deadlock
; The helper function will examine s for each box's position.
(defun is-deadlock-helper (s boxes)
	(cond
		((null boxes) nil)
		(t 
			(let*
				((box (car boxes))
				(r (first box))
				(c (second box)))
			(or
				(is-deadlock-at-position s (- r 1) (- c 1))
				(is-deadlock-at-position s (- r 1) c)
				(is-deadlock-at-position s r (- c 1))
				(is-deadlock-at-position s r c)
				(is-deadlock-helper s (cdr boxes))
			)
		))
	)
)

(defun is-deadlock (s)
	(let
		((boxes (find-boxes s)))
	(is-deadlock-helper s boxes)
))

; If it is deadlock, assign a high number.
(defun h-deadlock (s)
	(cond
		((is-deadlock s) 2000)
		(t 0)
	)
)

; Box to goal heuristics

; Return a list of (r c) positions of all boxes
(defun find-boxes-helper (s r c accum)
	(let*
		((p (list r c))
		(this-row (cond ((null s) nil) (t (car s))))
		(this-cell (cond ((null this-row) nil) (t (car this-row)))))
	(cond
		((null s) accum)
		((null this-row) (find-boxes-helper (cdr s) (+ r 1) 0 accum))
		(t (cond
			((isBox this-cell) (find-boxes-helper (cons (cdr this-row) (cdr s)) r (+ c 1) (cons p accum)))
			(t (find-boxes-helper (cons (cdr this-row) (cdr s)) r (+ c 1) accum))
		))
)))

(defun find-boxes (s)
	(find-boxes-helper s 0 0 '())
)

; Return a list of (r c) positions of all goals
(defun find-goals-helper (s r c accum)
	(let*
		((p (list r c))
		(this-row (cond ((null s) nil) (t (car s))))
		(this-cell (cond ((null this-row) nil) (t (car this-row)))))
	(cond
		((null s) accum)
		((null this-row) (find-goals-helper (cdr s) (+ r 1) 0 accum))
		(t (cond
			((or (isStar this-cell) (isKeeperStar this-cell)) (find-goals-helper (cons (cdr this-row) (cdr s)) r (+ c 1) (cons p accum)))
			(t (find-goals-helper (cons (cdr this-row) (cdr s)) r (+ c 1) accum))
		))
)))

(defun find-goals (s)
	(find-goals-helper s 0 0 '())
)

; Simple distance from the position (r1, c1) to (r2, c2)
(defun simple-distance (r1 c1 r2 c2)
	(let 
		((r-distance (cond ((> r1 r2) (- r1 r2)) (t (- r2 r1))))
		(c-distance (cond ((> c1 c2) (- c1 c2)) (t (- c2 c1)))))
	(+ r-distance c-distance)
))
(defun simple-distance-p (p1 p2)
	(simple-distance (first p1) (second p1) (first p2) (second p2))
)

; List of distances from a position to all goals
(defun distance-to-goals-helper (s p goals accum)
	(cond
		((null goals) accum)
		(t (distance-to-goals-helper s p (cdr goals) (cons (simple-distance-p p (car goals)) accum)))
	)
)
(defun distance-to-goals (s p)
	(let
		((goals (find-goals s)))
	(distance-to-goals-helper s p goals '())
))

; This heuristics is calculated as followed:
; Find distance of each box to the closest goal
; Sum of all such distances is the heuristics
(defun h-box-to-goal-helper (s boxes accum)
	(let 
		((goals (find-goals s)))		
	(cond 
		((null boxes) accum)
		(t (h-box-to-goal-helper s (cdr boxes) (+ accum (min-list (distance-to-goals s (car boxes)) 0))))
	)
))

(defun h-box-to-goal (s)
	(let 
		((boxes (find-boxes s)))
	(h-box-to-goal-helper s boxes 0)		
))

; Keeper to box heuristics

; List of distances from a position to all boxes
(defun distance-to-boxes-helper (s p boxes accum)
	(cond
		((null boxes) accum)
		(t (distance-to-boxes-helper s p (cdr boxes) (cons (simple-distance-p p (car boxes)) accum)))
	)
)
(defun distance-to-boxes (s p)
	(let
		((boxes (find-boxes s)))
	(distance-to-boxes-helper s p boxes '())
))
; The number of moves require to move the keeper to each box
; is the distance minus one
(defun moves-to-boxes-helper (distances accum)
	(cond
		((null distances) accum)
		(t (moves-to-boxes-helper (cdr distances) (cons (- (car distances) 1) accum)))
	)
)
(defun moves-to-boxes (s p)
	(let
		((distances (distance-to-boxes s p)))
	(moves-to-boxes-helper distances '())
))

; Note to self: This is a trash heuristic. Because 
; this heuristis discourage the keeper from pushing the
; box into the goal 

; This heuristics is calculated as followed:
; The distance from the keeper the closest box, thus encourage
; him to move to a box
(defun h-keeper-to-box (s)
	(let* 
		((the-keeper (getKeeperPosition-rc s 0))
		(closest-distance (min-list (moves-to-boxes s the-keeper) 0)))
	(clamp closest-distance 0 999999)
))

; Box to goal matching heuristics

; The important positions are: This, and two squares ahead of it, in direction.
; Return the tuples (r c) for positions.
(defun get-three-important-positions-at-p (p direction)
	(let* 
		((r (first p))
		(c (second p)))
	(cond 
		((equal direction 'up) 
			(list 
				p 
				(list (- r 1) c)
				(list (- r 2) c)
			)
		)
		((equal direction 'down) 
			(list 
				p 
				(list (+ r 1) c)
				(list (+ r 2) c)
			)
		)
		((equal direction 'left) 
			(list 
				p 
				(list r (- c 1))
				(list r (- c 2))
			)
		)
		((equal direction 'right) 
			(list 
				p 
				(list r (+ c 1))
				(list r (+ c 2))
			)
		)
	)
))

; Get next position, to inserted into the queue
; Return a pair, (pos1, pos2). 
; If pos1 is nil, that position is invalid
(defun get-next-position (s p map direction)
	(let* 
		((three-positions (get-three-important-positions-at-p p direction))
		(position1 (second three-positions))
		)
	(cond	
		; check if it is already visited
		((< (get-square-p map position1) 10000) nil)
		; Check if the objects ahead is a not a wall. 
		(t
			(let*
				((object1 (get-square-p s position1)))
			(cond
				((isNil object1) nil)
				((isWall object1) nil)
				(t 
					(let*
						((position2 (second three-positions))
						(object2 (get-square-p s position2))
						)
					(cond 
						((isNil object2) nil)
						((isWall object2) nil)
						; Good, we return that position
						(t position1)
					))
				)
			))
		)
	)
))

; Queue will store the tuples (r c) as positions
; Unvisited node will have a very large number.
; cost-entry is in the format (goal, map), with map has identical structure as s, stored the 
; distances from goal to a position, very large number if unreachable. Note that the function
; accumulates map, but it will return a cost-entry
(defun pushing-distance-from-goal-helper (s gpos queue map)
	(let*
		((top (queue-pop queue))
		(this-position (first top))
		(queue0 (second top)))
	(cond
		((null top) (list gpos map))
		(t (let*
			(
			(pos-up (get-next-position s this-position map 'up))
			(pos-down (get-next-position s this-position map 'down))
			(pos-left (get-next-position s this-position map 'left))
			(pos-right (get-next-position s this-position map 'right))
			(cost (get-square-p map this-position))
			(queue1 (cond ((null pos-up) queue0) (t (queue-push pos-up queue0))))
			(queue2 (cond ((null pos-down) queue1) (t (queue-push pos-down queue1))))
			(queue3 (cond ((null pos-left) queue2) (t (queue-push pos-left queue2))))
			(queue4 (cond ((null pos-right) queue3) (t (queue-push pos-right queue3))))
			(map1 (cond ((null pos-up) map) (t (set-square-p map pos-up (+ cost 1)))))
			(map2 (cond ((null pos-down) map1) (t (set-square-p map1 pos-down (+ cost 1)))))
			(map3 (cond ((null pos-left) map2) (t (set-square-p map2 pos-left (+ cost 1)))))
			(map4 (cond ((null pos-right) map3) (t (set-square-p map3 pos-right (+ cost 1)))))
			)
		(pushing-distance-from-goal-helper s gpos queue4 map4)
		))
	)
))

; This takes a goal as parameters, return a list consist of
; the tuples in this format
; ((goal-row goal-column) (row column) d)
; d is the distance from the goal to a position
; Very large number if that position is unreachable
(defun pushing-distance-from-goal (s gpos)
	(let*
		((queue0 (queue-new))
		(queue1 (queue-push gpos queue0))
		(map0 (populate s 99999))
		(map1 (set-square-p map0 gpos 0)))
	(pushing-distance-from-goal-helper 
		s gpos queue1 map1)
))


; Filter the distances return from pushing-distance-from-goal,
; so that only the distances to boxes are return.
; It will convert the (goal, map) format to (goal, box, distance)
; format to cut the unnecessary information and save space.
(defun convert-distance-from-goal (distance-table boxes accum)
	(cond
		((null boxes) accum)
		(t (let*
			((gpos (first distance-table))
			(map (second distance-table))
			(this-box (car boxes))
			(cost (get-square-p map this-box))
			(entry (list gpos this-box cost)))
		(convert-distance-from-goal distance-table (cdr boxes) (cons entry accum))
		))
	)
)

; Helper for pushing-distance
(defun pushing-distance-helper (s goals boxes accum)
	(cond
		((null goals) accum)
		(t
			(let*
				((this-goal (car goals))
				(distances (pushing-distance-from-goal s this-goal))
				(accum1 (convert-distance-from-goal distances boxes accum))
				)
			(pushing-distance-helper s (cdr goals) boxes accum1)
			)
		)
	)
)

; Return a list consist of 
; (goal-position box-position distance)
(defun pushing-distance (s)
	(let*
		((boxes (find-boxes s))
		(goals (find-goals s))) 
	(pushing-distance-helper s goals boxes '())
))

; Lookup the distance between a goal and a box
; If not exist, return a very large number
(defun lookup (goal box distances)
	(let
		((elem
			(cond
				((null distances) nil)
				(t (car distances))
			)
		))
	(cond
		((null elem) 999999)
		((and (equal goal (first elem)) (equal box (second elem))) (third elem))
		(t (lookup goal box (cdr distances)))
	)
))

; To assign goals to boxes, we try all combinations,
; to see which combination has smallest total cost
; We'll use dfs for assignment
; Warning: It has O(n!) complexity relative to 
; number of boxes

; Return a list of new nodes
; node: (goal box unassigned-goals unassigned-boxes cost-so-far)
(defun expand (node cost-table boxes-to-assign accum)
	(let
		((unassigned-goals (third node))
		(unassigned-boxes (fourth node))
		(cost-so-far (fifth node)
		)
	)
	(cond
		((null unassigned-goals) nil)
		((null boxes-to-assign) accum)
		(t (expand 
			node 
			cost-table
			(cdr boxes-to-assign) 
			(cons (list (car unassigned-goals) (car boxes-to-assign) (cdr unassigned-goals) (rm (car boxes-to-assign) unassigned-boxes) (+ cost-so-far (lookup (car unassigned-goals) (car boxes-to-assign) cost-table))) accum))
		)
	)
))

; We can use the stack to traverse the tree dfs style
(defun dfs (stack cost-table all-boxes minimum-cost)
	(cond
		((null stack) minimum-cost)
		(t (let*
			((pop (car stack))
			; If there's no unassigned goals, meaning there're all assignments ready, we'll recalculate the minimum cost
			(unassigned-goals (third pop))
			(unassigned-boxes (fourth pop))
			(cost-so-far (fifth pop))
			(minimum-cost1 (cond ((and (null unassigned-goals) (< cost-so-far minimum-cost)) cost-so-far) (t minimum-cost)))
			; If there're still unassigned-goals, expand the new nodes
			(expansion (expand (car stack) cost-table unassigned-boxes '()))
			(stack1 (append expansion (cdr stack))))
		(dfs stack1 cost-table all-boxes minimum-cost1)
		))
	)
)

; This define the first element to put in the dfs stack
(defun dfs-first-element (goals boxes)
	(list (list nil nil goals boxes 0))
)


(defun assign (s)
	(let*
		((boxes (find-boxes s))
		(goals (find-goals s))
		(distances (pushing-distance s))
		(result (cond
			((null boxes) 0)
			((null goals) 0)
			(t (dfs (dfs-first-element goals boxes) distances boxes 999999))
		))
		)
	result			
))

(defun h-assign (s)
	(let*
		((result (assign s)))
	(clamp result 0 1000)
))

; Simple assign heuristics
; A hybrid between h-assign and h-box-to-goal
; Simply use the simple-distance between boxes and goals, unless a 
; (goal, box) pair is not reachable.
; Then we do the matching.

; similar to pushable-distance-from goal, but we can use a stack
; to improve the speed.
; accum stores the simple-distance from the goal to the boxes
(defun reachable-from-goal-helper (s gpos stack visited boxes accum)
	(cond
		((null stack) accum)
		((null boxes) accum)
		(t (let*
			((this-position (car stack))
			(is-it-a-box (in this-position boxes))
			(accum1 (cond (is-it-a-box (cons (list gpos this-position (simple-distance-p gpos this-position)) accum)) (t accum)))
			(boxes1 (cond (is-it-a-box (rm this-position boxes)) (t boxes)))
			(pair-up (get-next-position s this-position visited 'up))
			(pos-up (first pair-up))
			(pair-down (get-next-position s this-position visited 'down))
			(pos-down (first pair-down))
			(pair-left (get-next-position s this-position visited 'left))
			(pos-left (first pair-left))
			(pair-right (get-next-position s this-position visited 'right))
			(pos-right (first pair-right))
			(stack0 (cdr stack))
			(stack1 (cond ((null pos-up) stack0) (t (cons pos-up stack0))))
			(stack2 (cond ((null pos-down) stack1) (t (cons pos-down stack1))))
			(stack3 (cond ((null pos-left) stack2) (t (cons pos-left stack2))))
			(stack4 (cond ((null pos-right) stack3) (t (cons pos-right stack3))))
			(visited1 (set-square-p visited this-position 1))
			)
				(reachable-from-goal-helper s gpos stack4 visited1 boxes1 accum1)
		))
	)
)

(defun reachable-from-goal (s gpos boxes accum)
	(let*
		((visited0 (populate s 0))
		(visited1 (set-square-p visited0 gpos 1)))
	(reachable-from-goal-helper 
		s gpos (list gpos) visited1 boxes accum)
))

(defun reachable-distance-helper (s goals boxes accum)
	(cond
		((null goals) accum)
		(t
			(let*
				((this-goal (car goals))
				(accum1 (reachable-from-goal s this-goal boxes accum)))
			(reachable-distance-helper s (cdr goals) boxes accum1)
		))
	)
)

(defun reachable-distance (s)
	(let*
		((boxes (find-boxes s))
		(goals (find-goals s))) 
	(reachable-distance-helper s goals boxes '())
))

(defun assign-hybrid (s)
	(let*
		((boxes (find-boxes s))
		(goals (find-goals s))
		(distances (reachable-distance s)))
	(cond
		((null boxes) 0)
		((null goals) 0)
		(t (dfs (dfs-first-element goals boxes) distances boxes 999999))
	)
))

; Not good. h-assign-hybrid performs worse than h-assign on tests
(defun h-assign-hybrid (s)
	(let*
		((result (assign-hybrid s)))
	(clamp result 0 1000)
))
  

; Other helper functions
; Clamp n between lowerbound and upperbound
(defun clamp (n lower upper)
	(cond
		((> lower upper) nil)
		((> n upper) upper)
		((< n lower) lower)
		(t n)
	)
)

; Find the minimum element in a list
(defun min-list-helper (lst m)
	(cond 
		((null lst) m)
		((< (car lst) m) (min-list-helper (cdr lst) (car lst)))
		(t (min-list-helper (cdr lst) m))
	)
)
(defun min-list (lst default)
	(cond
		((null lst) default)
		(t (min-list-helper lst 999999)) 
	)
)

; Find the maximum element in a list
(defun max-list-helper (lst m)
	(cond 
		((null lst) m)
		((> (car lst) m) (max-list-helper (cdr lst) (car lst)))
		(t (max-list-helper (cdr lst) m))
	)
)
(defun max-list (lst default)
	(cond
		((null lst) default)
		(t (max-list-helper lst -999999)) 
	)
)

; Check if an element is in a list
(defun in (elem lst)
	(cond
		((null lst) nil)
		((equal elem (car lst)) t)
		(t (in elem (cdr lst)))
	)
)

; Remove an element from the list if it exists
(defun rm-helper (elem lst accum)
	(cond
		((null lst) accum)
		((equal elem (car lst)) (rm-helper nil (cdr lst) accum))
		(t (rm-helper elem (cdr lst) (cons (car lst) accum)))
	)
)
(defun rm (elem lst)
	(rm-helper elem lst '())
)

; A special implementation of queue, that make it more efficient
; The idea is that maintaining the front part and the back part
; of the queue. The back part are in reverse.
; When pushing, simply cons it to the front part
; When popping, simply car the back part. If the back part is 
; empty, just transfer the front part to the back part.
(defun queue-new ()
	(list '() '())
)

; Return the new queuequeue-new
(defun queue-push (elem q)
	(let 
		((front (first q))
		(back (second q)))
	(list (cons elem front) back)
))

; Reverse a list
(defun reverse-list (lst accum)
	(cond
		((null lst) accum)
		(t (reverse-list (cdr lst) (cons (car lst) accum)))
	)
)

; Return a tuple: the element and the new queue
(defun queue-pop (q)
	(let 
		((front (first q))
		(back (second q)))
	(cond
		; Both front and back are null, queue is empty
		((and (null back) (null front)) nil)
		; Back is null, transfer the front to the back part
		((null back) 
			(let*
				((new-back (reverse-list front '()))
				(elem (car new-back))
				(remain (cdr new-back))
				(new-q (list '() remain)))
			(list elem new-q)
		))
		; Else just take it out the back part
		(t 
			(let*
				((elem (car back))
				(remain (cdr back))
				(new-q (list front remain)))
			(list elem new-q)
		))
	)
))

; Return an array with same size of s, populated with value v
(defun populate-helper-row (v w accum)
	(cond
		((= w 0) accum)
		(t (populate-helper-row v (- w 1) (cons v accum)))
	)
)

(defun populate-helper (v h the-row accum)
	(cond
		((= h 0) accum)
		(t (populate-helper v (- h 1) the-row (cons the-row accum)))
	)
)

(defun populate (s v)
	(let* 
		((h (length s))
		(w (length (car s)))
		(the-row (populate-helper-row v w '())))
	(populate-helper v h the-row '())	
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
  