;;;; GLOBALS

(let ((*stack* nil)	; STRIPS stack
      (*history* nil)	; history for backtracking
      (*ops* nil)	; current partial plan
      (*bindings* (make-binding-structure)) ; current bindings
      (*debug* t))

;;;; ================================================================= 

;;;; Debugging functions

(defun stack () *stack*)
(defun history () *history*)
(defun ops () *ops*)
(defun bindings ()
  (maphash #'(lambda (var val)
               (print (list var val)))
           *bindings*))

(defun strips-debug (&optional (mode t)) 
(setf *debug* mode))

;;;; ================================================================= 

;;;; Binding functions.

;;; Bindings are made globally, stored on the 
;;; list *bindings*. INSTANTIATE takes a formula and returns a copy
 ;;; of that formula with substitutions for all variables that are
 ;;; currently bound.

(defun instantiate (formula)
  (cond ((var-p formula)
	 (let ((val (binding-structure-get (var-name formula)
                                           *bindings*)))
           (if val (instantiate val) formula)))
	((atom formula)
	 formula)
	((listp formula)
	 (cons (instantiate (car formula))
               (instantiate (cdr formula))))))

(defun instantiate-wff (formula bindings)
  (cond ((var-p formula)
	 (let ((val (binding-structure-get (var-name formula)
                                           bindings)))
           (if val (instantiate-wff val bindings) formula)))
	((atom formula)
	 formula)
	((listp formula)
	 (cons (instantiate-wff (car formula) bindings)
               (instantiate-wff (cdr formula) bindings)))))

(defun make-binding (binding)
  (binding-structure-add binding *bindings*) 
  (push (binding-event-note binding) *history*)) 

;;; Note that this is not symmetric with MAKE-BINDING. MAKE-BINDING
;;; is an event, which adds a note to the history so it can be 
;;; undone. RETRACT-BINDING is a utility function that is used only
;;; when backtracking to undo an MAKE-BINDING. 

(defun retract-binding (binding)
  (binding-structure-delete binding *bindings*)) 

;;;; ========================================================== 

;;;; Top Level

;;; STRIPS puts the initial goal on its stack and loops doing the
;;; following:
;;;	IF the top of the stack is a goal
;;;	THEN IF there are operators that would achieve the goal
;;;	THEN push a list of applicable operators onto the stack
;;;	ELSEIF the goal is conjunctive
;;;	THEN split it into subgoals and put them on the stack
;;;	ELSE backtrack
;;;	ELSEIF the top of the stack is an operator
;;;	THEN add to plan and simulate execution
;;;	ELSEIF the top of the stack is a possibilities list
;;;	THEN pop the first possibility and put it on the stack

(defun strips (goal)
  (setf *stack* (list (make-goal goal)))
  (loop (when (null *stack*) (return))
        (if *debug* (print-stack *stack*))
        (if (equal (read-char-no-hang) #\b) (break)) ; for debugging
        (cond ((goal-p (car *stack*))
               (or (plan) (split-goals) (backtrack))) 	 
              ((op-p (car *stack*))
               (execute-op))
              ((possibilities-listp (car *stack*))
               (try-next))))
  (print-plan *ops*)
  (prog1 (instantiate *ops*) (reset-planner))) 


;;; PLAN produces a possibilities list consisting of all ways to 
;;; SHOW a goal followed by all ways to achieve it (see description
;;; of possibilities list, below).

(defun plan ()
  (let* ((goal (car *stack*))
	 (possibilities (append (show-goal goal) ; All ways to show
				(fetch-operators goal)))) ; All ways to do 
    (when possibilities
          (pop *stack*)
          (push (remove-goal-event-note goal) *history*) 	 
          (push possibilities *stack*))))

;;; SPLIT-GOALS breaks up a conjunctive goal. 

(defun split-goals ()
  (let ((goal (car *stack*)))
    (if (conjunctive-p (goal-state goal))
	(for (subgoal-state :in (reverse (cdr (goal-state goal))))
             :do (let ((subgoal (make-goal subgoal-state))) 		
                   (push (add-goal-event-note subgoal) *history*) 	
                   (push subgoal *stack*))))))

;;; BACKTRACK does a series of "undos" (popping the history 
;;; stack) until it gets to a possibilities list, i.e., the last choice 
;;; point.

(defun backtrack ()
  (loop
   (when (null *history*) (break))	; total failure
   (let ((event-note (pop *history*)))
     (when (and (possibilities-list-event-note-p event-note)
                (not (null event-note)))
           (push event-note *stack*)
           (return))                    ; normal exit
     (cond ((execute-event-note-p event-note)
            (push (pop *ops*) *stack*)	; retract a scheduled
            (retract-operator event-note)) ; operator
           ((add-goal-event-note-p event-note)
            (setf *stack* (remove event-note *stack*)))
           ((remove-goal-event-note-p event-note) 		
            (push (cadr event-note) *stack*))
           ((add-op-event-note-p event-note)
            (setf *stack* (remove (cadr event-note) *stack*)))
           ((binding-event-note-p event-note) ; undo a binding 		
            (retract-binding (cadr event-note) ))))))

;;; TRY-NEXT gets the next item on a possibilities list (see below), 
;;; makes the bindings it specifies, pushes the operator it specifies (if
;;; any) onto the stack, pushes the operator's precondition onto the stack
;;; and puts the remainder of the possibilities list onto the history list. 

(defun try-next ()
  (let* ((poss-list (pop *stack*))
	 (next-poss (car poss-list))
	 (op (poss-pair-op next-poss))
	 (binds (poss-pair-binds next-poss)))
    (push (cdr poss-list) *history*)
    (for (binding :in binds)
	 :do (make-binding binding))
    (when op
          (push op *stack*)
          (push (add-op-event-note op) *history*)
          (if (op-precondition op)
              (let ((subgoal (make-goal (op-precondition op))))       
                (push (add-goal-event-note subgoal) *history*)
                (push subgoal *stack*))))))

;;; EXECUTE-OP takes an operator and simulates execution by deleting the
;;; deletes and adding the adds (including the goal). 
;;; -- modified this operator and commented out the old definition
;;; -- execute, because it redefines the CLISP function execute
;;; -- vladimir kulyukin

;;; N.B. -- The meaning of an item on the delete list is problematic.
;;; Since it can have variables in it, the interpretation is that 
;;; anything it matches in the data base should be deleted. However, 
;;; it is not clear what to do if it matches multiple items. The 
;;; problem is that the bindings made in the match are often important 
;;; to the operator (for example, this is how we might pick up the 
;;; old location of an item that we are moving), but it is not clear 
;;; what to do if we have a list of alternative bindings for a variable. 
;;; Normally this would come up, because usually what we are deleting is 
;;; an assertion about some state variable that can only have one value, 
;;; like the location of an object. However, it is not impossible to 
;;; imagine a use that depends on matching and deleting a set of 
;;; assertions, like the assertion that an exploding bomb kills anyone 
;;; who is near enough to it. Ideally, we would like to allow this, but
;;; the mechanics of manipulating the list of resulting bindings would 
;;; be very awkward. The approach here is to assume that only one 
;;; formula will match each item on the delete list. If multiple 
;;; formulas match, only the first is deleted, and this match provides 
;;; the bindings for any free variables. 

;(defun execute ()
;  (let ((op (pop *stack*)))
;    (format t "~%APPLYING operator => ~A~%" (instantiate (op-action op))) 
;    (for (delete :in (instantiate (op-delete op))) 	 
;         :do (let ((binds (retract delete)))
;               (for (bind :in binds)
;                    :do (make-binding bind))))
;    (store (instantiate (op-goal op)))
;    (for (add :in (op-add op))
;	 :do (let ((inst-add (instantiate add)))
;               (if (no-vars-in inst-add)
;                   (store inst-add))))
;    (push op *ops*)
;    (push (execute-event-note op) *history*)))

(defun execute-op ()
  (let ((op (pop *stack*)))
    (format t "~%APPLYING operator => ~A~%" (instantiate (op-action op))) 
    (for (delete :in (instantiate (op-delete op))) 	 
         :do (let ((binds (retract delete)))
               (for (bind :in binds)
                    :do (make-binding bind))))
    (store (instantiate (op-goal op)))
    (for (add :in (op-add op))
	 :do (let ((inst-add (instantiate add)))
               (if (no-vars-in inst-add)
                   (store inst-add))))
    (push op *ops*)
    (push (execute-event-note op) *history*)))


(defun reset-planner ()
  (for (op :in *ops*)
       :do (retract-operator op t))
  (setf *stack* nil)
  (setf *history* nil)
  (setf *ops* nil)
  (setf *bindings* (make-binding-structure))) 

;;;; ================================================================== 

;;;; Data types

;;; A POSSIBILITIES-LIST is a list of possibilities for achieving
;;; a goal. Each item of the list is a POSS-PAIR, which is a pair
;;; of the form (<operator> <bindings>). The idea is that if the 
;;; given bindings are assumed, the given operator will achieve the
;;; goal. The goal may be NIL, meaning the goal is already true 
;;; if the bindings are assumed. Possibilities lists are the choice
;;; points over which the planner backtracks. 

(defun possibilities-listp (item)
  (and (listp item)
       (every #'poss-pair-p item)))

(defun make-poss-pair (op binds)
  (list op binds))

(defun poss-pair-op (item)
  (assert (poss-pair-p item))
  (car item))

(defun poss-pair-binds (item)
  (assert (poss-pair-p item))
  (cadr item))

(defun poss-pair-p (item)
  (and (listp item)
       (or (not (car item))
           (op-p (car item)))
       (binding-listp (cadr item))))

;;; A BINDING-LIST is a list of binding pairs, where each pair is of the 
;;; form (<var-name> <value>).

(defun binding-listp (item)
  (and (listp item)
       (every #'binding-p item)))

(defun binding-p (item)
  (or (not item) (and (listp item) (= (length item) 2)))) 

;;; A GOAL is a goal for the planner, which is just a predication 
;;; with a :GOAL slapped on the front so we know what it is. 

(defun make-goal (fact)
  (list ':goal fact))

(defun goal-state (goal)
  (cadr goal))

(defun goal-p (item)
  (and (listp item)
       (eq (car item) ':goal)))

;;; EVENT-NOTES are the items that are put on the history list to 
;;; allow backtracking. There are four basic notable events: 
;;; binding a variable, simulating the execution of an operator, 
;;; choosing an item from a possibilities list (see above), and 
;;; de-activating a goal (i.e. deciding that it has been planned for). 

;;; An ADD-GOAL event note notes that a goal was put on the stack 

(defun add-goal-event-note (goal)
  goal)

(defun add-goal-event-note-p (item)
  (goal-p item))

;;; A REMOVE-GOAL event note notes that a goal was removed from the
;;; stack (at the time a possibilities list was generated for it. 

(defun remove-goal-event-note (goal)
  (list ':remove-goal goal))

(defun remove-goal-event-note-p (item)
  (and (listp item)
       (eq (car item) ':remove-goal)))

;;; An ADD-OP event note notes that an operator was put on the stack
;;; N.B. -- it may have come off to be executed. We'll ignore that 
;;; since the backtrack event for adding an OP is a no-op when the OP 
;;; isn;t on the stack

(defun add-op-event-note (op)
  (list ':add-op op))

(defun add-op-event-note-p (item)
  (and (listp item)
       (eq (car item) ':add-op)))



;;; A BINDING-EVENT-NOTE notes that a variable was bound to a given
;;; value.

(defun binding-event-note (binding)
  (list ':binding-event-note binding))

(defun binding-event-note-p (item)
  (and (listp item)
       (eq (car item) ':binding-event-note)))

;;; An EXECUTE-EVENT-NOTE notes that the planner simulated the execution
;;; of an operator.

(defun execute-event-note (op)
  op)

(defun execute-event-note-p (item)
  (op-p item))


;;; A POSSIBILITIES-LIST-EVENT-NOTE notes that the planner tried the
;;; first item on a possibilities list. The event note stores the 
;;; rest of the possibilities list.

(defun possibilities-list-event-note-p (item) 
  (possibilities-listp item))

;;;; =============================================================== 

;;;; Utility functions

;;; SHOW-GOAL finds all ways that the goal can be SHOWn -- i.e., finds 
;;; all sets of bindings under which it is currently true. The 
;;; result is a possibilities-list (see above), which explains the 
;;; odd construction of the list that is formed. 

(defun show-goal (goal)
  (for (bl :in (show (instantiate (goal-state goal)))) 
       :save (list nil bl)))

;;; FETCH-OPERATORS finds all operators that could achieve the 
;;; goal under any set of assumed bindings. This is not as simple 
;;; as you might think at first: Note that there is not one entry per 
;;; applicable operator, but rather one per workable operator/binding list 
;;; pair -- that is, if an operator would work with different sets 
;;; of bindings, we generate one result for each of these sets of 
;;; bindings. The different sets of bindings are generated by 
;;; different ways of SHOWing the filter conditions of the operator. 
;;; The result is in the form of a possibilities-list (see above). 

;;; N.B. -- Notice that each operator is UNIQUIFIED before it is 
;;; checked. This is a critical step. Uniquify in effect renames 
;;; all of the variables in a formula to names that are guaranteed
;;; to be unique. This prevents the planner from confusing variables 
;;; with the same name in different operators (or in the same operator 
;;; if it is applied more than once in the plan). The need for this is 
;;; a consequence of the decision to have a global binding list. 

(defun fetch-operators (goal)
  (let ((binds nil)
	(op-inst nil)
	(goal-state (instantiate (goal-state goal)))) 
    (for (op :in (fetch-op goal-state))
         :when (and (setf op-inst (uniquify op))
                    (setf binds (unify goal-state (op-goal op-inst)))) 
         :splice
         (if (null (op-filter op-inst))
             (list (list op-inst binds))
           (for (fbind :in
                       (show (instantiate
                              (make-subst (op-filter op-inst) binds))))
                :save (list op-inst
                            (union binds fbind)))))))

;;; RETRACT-OPERATOR is a utility function used in backtracking. 
;;; It retracts the items on the operator's add list, and adds the 
;;; items on the operator's delete list. The history list does not 
;;; explicitly store the bindings that held when the operator was 
;;; initially applied; rather, it is assumed that through backtracking 
;;; the planner has returned to the context in which the application 
;;; was done, so that the current global bindings are the ones that 
;;; were in place then. See note on EXECUTE, above, for discussion 
;;; of the interpretation of the delete list. 

(DEFUN RETRACT-OPERATOR (operator &optional (silent-p nil)) 
  (let ((old-quiet-db *quiet-db*))
    (setf *quiet-db* t)
    (if (not silent-p)
	(format t "~%RETRACTING operator => ~A~%" 		
                (instantiate (op-action operator))))
    (retract (instantiate (op-goal operator))) 
    (for (add :in (instantiate (op-add operator))) 
	 :when (no-vars-in add)
         :do (if (not silent-p)
                 (format t " RETRACTING => ~A~%" add))
         (retract add))
    (for (delete :in (instantiate (op-delete operator)))
 	 :when (no-vars-in delete)
	 :do (store delete))
    (setf *quiet-db* old-quiet-db)))
	

;;;; ============================================================== 

;;;; Print functions

(defun print-plan (ops)
  (format t "PLAN:~%")
  (for (op :in (instantiate (reverse ops))) 
       :do (format t " ~A~%" (op-action op)))
  (format t "~%"))

(defun print-stack (stack)
  (for (item :in stack)
       :do (print-item item))
  (format t "~%_____________________________") 
  (format t "~%~%~%~%"))

(defun print-item (item)
  (cond ((goal-p item)
	 (format t "~% ~A" (instantiate item)))
 	((op-p item)
	 (format t "~% (OPERATOR ~A)" (instantiate (op-action item))))
 	((possibilities-listp item)
	 (format t "~% (POSSIBILITIES")
	 (for (poss :in item)
              :do (if (poss-pair-op poss)
                      (format t " ~A" (op-name (poss-pair-op poss)))
                    (format t " ~A" (poss-pair-binds poss))))
 	 (format t ")"))))

)

