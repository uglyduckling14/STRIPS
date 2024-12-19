;;; S-RULES

;;; This file contains the data base for the STRIPS problem solver.
;;; STRIPS data is in the form of operators that add and delete states 
;;; from a data-base describing the world.

;;; OPERATORS are list structures with variables.  They are saved in 
;;; the data base as assertions.  
;;; An important point --- the rules themselves violate FOPC syntax.


;;; ------------------------ DATA TYPES -----------------------

;;; OPERATORS have five fields of interest and a name
;;; ACTION - the action that will be in the plan when the operator is
;;;        run.
;;; ADD - a list of the WFFs that are added to the data base as a
;;;        result of the application of the operator
;;; DELETE - a list of the WFFs that are removed from the data base
;;;        as a result of application of the operator.
;;; PRECONDITIONS - a list of the WFFs that must be in the data base
;;;       in order to run the operator.
;;; FILTER - a list of the WFFs that must be in the data-base in order
;;;       to consider running the operator.

;;; There are six access functions for operators:

;;; OP-NAME

(DEFUN OP-NAME (operator) (cadr operator))

;;; OP-ACTION

(DEFUN OP-ACTION (operator) (cadr (member ':action operator)))

;;;  OP-GOAL

(DEFUN OP-GOAL (operator) (cadr (member ':goal operator)))

;;; OP-ADD

(DEFUN OP-ADD (operator) (cadr (member ':add operator)))

;;; OP-DELETE

(DEFUN OP-DELETE (operator) (cadr (member ':del operator)))

;;; OP-PRECONDITION

(DEFUN OP-PRECONDITION (operator) 
  (let ((pres (cadr (member ':precond operator))))
    (cond ((null pres) nil)
             ((eq (length pres) 1) (car pres))
             (t (cons 'and pres)))))


;;; OP-FILTER

(DEFUN OP-FILTER (operator)
   (let ((filts (cadr (member ':filter operator))))
    (cond ((null filts) nil)
             ((eq (length filts) 1) (car filts))
             (t (cons 'and filts)))))

;;; And one test

(DEFUN OP-P (item) (and (consp item) (eq (car item) ':operator)))


(defun print-operator (operator stream level)
 (princ (list 'operator (operator-name operator))))


(defun show-operator (operator)
 (format t "(~A~%ACTION = ~%~A~%ADDS =~%~A~%"
              (op-name operator)(op-action operator)(op-add operator))
 (format t "DELETES =~%~A~%PRECONDITIONS =~%~A~%"
              (op-delete operator) (op-precondition operator))
 (format t "FILTER =~%~A~%" (op-filter operator)))

;;; A macro for building rule definitions.

;;; Opperators are lists with variables and are stored in the 
;;; data-base. For each ADD, the operator is stored as 
;;; (planfor ADD operator).

(DEFINDEX op 
#'(lambda (item) (op-goal item))
  :no-add (lambda (item1 item2) 
                (formula-equal item1 item2)))


(DEFMACRO DEF-OPERATOR (name &key
                        action 
                        goal
                        precond 
                        filter 
                        add
                        del)
  `(build-and-store-operator 
         ',name ',action ',goal ',precond ',filter ',add ',del))

(DEFUN BUILD-AND-STORE-OPERATOR (name act go pre fil add del)
  (let ((operator `(:operator ,name 
                             :action ,act
                             :goal ,go
                             :precond ,pre
                             :filter ,fil
                             :add ,add
                             :del ,del)))
     (index-op operator)
     name))

    
;;; Fetch-operators is handed a goal state.  It does a SHOW on 
;;; (PLANFOR <goal> ?operator) and then returns the MAKE-SUBST of 
;;; ?operator with the bindings. 
;;; It also checks filter conditions. 
;;; Fetch-operators returns multiple operators and multiple bindings.
;;; The actual value returned by fetch-operators is a list of the form
;;; (<operator> <bindings>)


;(DEFUN FETCH-OPERATORS (goal)
;  (let ((bindings nil)
;          (op-inst nil))
;     (for (op :in (fetch-op goal))
;         :when (if (setf bindings (unify goal (op-goal op)))
;                        (setf op-inst (make-subst op bindings)))
;        :splice
;            (if (null (op-filter op-inst))
;                 (list (list (list op-inst) bindings))
;                 (for (fbind :in (show (op-filter op-inst)))
;                   :save (list (list (make-subst op-inst fbind)) 
;                                    (union bindings fbind)))))))
 

;;; Code for the application of operators.  APPLY-OPERATOR takes an 
;;; operator, adds the assertions that the operator indicates in its 
;;; ADD (and deletes the ones it should delete).  Any time an operator 
;;; is applied, it is pushed on  the list of operators 
;;; *operators-applied*. When ever an operator is retracted, it is
;;;  popped off the stack.

;;; The world can be reset, using RESET-WORLD, which retracts all of  
;;; the operators.


(let ((*operators-applied* nil))

(DEFUN APPLY-OPERATOR (operator)
  (format t "~%APPLYING operator => ~A~%" (op-action operator)) 
  (for (delete :in (op-delete operator))
      :do (retract delete))
  (store (op-goal operator))
  (for (add :in (op-add operator))
      :do (store add))
  (push operator *operators-applied*)) 


;(DEFUN RETRACT-OPERATOR (operator)
;  (cond ((eq operator (car stack))
;            (format t "~%RETRACTING operator => ~A~%" 
;                         (op-action operator))
;            (retract (op-goal operator))
;            (for (add :in (op-add operator))
;               :do (retract add))
;            (for (delete :in (op-delete operator))
;               :do (store delete))
;            (pop *operators-applied*))
;           (t 
;            (format t "You cannot RETRACT an operator that you")
;            (format t " did not APPLY~%"))))

(DEFUN RESET-WORLD ()
  (format t "~%Resetting data-base...~%")
  (for (operator :in *operators-applied*)
       :do (format t "RETRACTING operator => ~A~%" 
                         (op-action operator))
            (retract (op-goal operator))
            (for (add :in (op-add operator))
                :do (retract add))
            (for (delete :in (op-delete operator))
                :do (store delete)))
  (setq *operators-applied* nil))
)

(defun retract (formula)
  (let ((binds nil)
	(datum nil))
    (for (dat :in (fetch-fact formula))
	 :first (if (setf binds (unify formula (datum-formula dat)))
		    (setf datum dat)))
    (if binds (erase-fact datum))
    binds))



(def-operator test1
   :action (test-act ?x ?y)
   :goal (foo ?x ?y)
   :precond ((baz ?x))
   :filter ((bar ?y))
   :add ((arg ?x ?y))
   :del ((grmph ?x ?y)))
