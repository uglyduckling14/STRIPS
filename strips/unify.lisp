
;;;  UNIFY.LISP

;;;  UNIFY
;;;  
;;;  A facility for unifying formulas.

;;; ------------------------ DATA TYPES ----------------------------------------

;;;   FORMULA 
;;;        A PC statement

;;; ------------------------ FUNCTIONS -----------------------------------------

;;;    UNIFY( pat1: <formula> pat2: <formula> ) 
;;;          => <binding list> or NIL

;;;    MAKE-SUBST( pat: <formula> binding-list: <binding-list> ) 
;;;          => <formula>

;;;    FORMULA-EQUAL( form1: <formula> form2: <formula> )
;;;          => <boolean>

;;;    SUBSUMES( form1: <formula> form2: <formula> )
;;;          => <boolean>

;;; ---------------------------- MACROS ----------------------------------------

;;; The character "?" followed by an atom  defines a variable.


;;; ADD-BINDING takes a variable, pattern and binding-list and adds the list
;;; (variable pattern) to the binding-list.


(DEFUN ADD-BINDING (var pat binding-list)
  (let ((binding (list (var-name var) pat)))
    (if (equal binding-list '(nil))
        (list binding)
        (cons binding binding-list))))

;;;  The binding in bdgs asscoiated with var.

(DEFUN VAR-BINDING (var bdgs)
  (binding-value (assoc (var-name var) bdgs)))

;;; The value of a VAR/BINDING list.

(DEFUN BINDING-VALUE (binding) (cadr binding))


;;; ------------------------ FUNCTIONS ------------------------------------

;;; UNIFY compares two formulas, and returns NIL if they do not unify, or
;;; the variable bindings that will make the formulas unify.  Each binding 
;;; is in the form of a list, whose CAR is the variable name, and whose CDR
;;; is the term to which the variable is bound.
;;;
;;; If the formulas unify, but no variables are bound, we have a slight
;;; problem: returning an empty binding list (i.e., NIL) would mistakenly
;;; signal a failure to the calling function.  To avoid this, we add an
;;; EMPTY BINDING to the binding list to begin with.  This means that
;;; when unification succeeds with no bindings, a binding list with
;;; the empty binding in it is returned (i.e., (NIL) ).  This is not
;;; equal to NIL, so it solves our problem.  The empty binding does
;;; not otherwise affect functions that manipulate the binding list.
;;;
;;; Here are some examples of what UNIFY returns:
;;;
;;; (UNIFY '(Man ?x) '(Man Socrates))  =>  ((X SOCRATES))
;;; (UNIFY '(Man Socrates) '(Man Socrates))  =>  (NIL)
;;; (UNIFY '(Man ?x) '(Mortal Socrates))  =>  NIL
;;;
;;; UNIFY's logic is as follows:
;;;     If the patterns are equal, then return the current bindings (success)
;;;     If either pattern is a variable, returns the results of VAR-UNIFY.
;;;     If either pattern is an atom, return nil (we already know that they aren't
;;;      equal
;;;     If the two patterns are different lengths, return nil (to avoid testing)
;;;     Finally, if each sublist in the patterns unify, return the resulting
;;;      bindings


(DEFUN UNIFY (pat1 pat2 &optional bindings)
  (if (null bindings) (setf bindings '(NIL)))
  (cond ((equal pat1 pat2) bindings)
        ((var-p pat1) (var-unify pat1 pat2 bindings))
	((var-p pat2) (var-unify pat2 pat1 bindings))
        ((or (atom pat1) (atom pat2)) nil)
	((not (= (length pat1) (length pat2))) nil)
        ((setf bindings (unify (car pat1) (car pat2) bindings))
         (unify (cdr pat1) (cdr pat2) bindings))
        (t nil)))


;;; UNIFY-VARS tries to unify a variable against a pattern.  
;;; First the binding of the variable is resolved (the binding is found)
;;;   If the variable is the same as the pattern, then just return the bindings.
;;;   If the variable is bound, try to unify it with the pattern.
;;;   If the variable is in the pattern (e.g. ?x and (head-of ?x)), return nil
;;;   Otherwise add the binding to the binding list.
;;;
;;;   By testing if the variable is the same as the pattern, we avoid ever adding
;;;   a binding of a variable to itself.

(DEFUN VAR-UNIFY (var pat bindings)
  (let ((old-binding (var-binding var bindings)))
    (cond ((var-equal var pat) bindings)
          (old-binding (unify pat old-binding bindings)) 
          ((occurs-in var pat bindings) nil)
	  (t (add-binding var pat bindings)))))


;;; OCCURS-IN is a test used to avoid binding a variable to a list that contains
;;; it.  
;;;     If the pattern is a variable, then return their identity or the results
;;;      of OCCURS-IN on the resolution of the variable.
;;;     If the pattern is an atom then return nil.
;;;     Otherwise check to see if the variable occurs-in the car or the cdr.

(DEFUN OCCURS-IN (var pat bindings)
  (cond ((var-p pat)
	 (or (var-equal var pat)
             (eq var (var-name pat))
	     (occurs-in var (var-binding pat bindings) bindings)))
	((atom pat) nil)
	((or (occurs-in var (car pat) bindings)
             (occurs-in var (cdr pat) bindings)))))
 

;;;  MAKE-SUBST replaces the variables in a pattern with its bindings.
;;;     If the pattern is a variable, establish its replacement given the bindings.
;;;     If the pattern is an atom, return it.
;;;     Otherwise return the CONS of the substitution of the car and cdr of the
;;;       pattern. 

(DEFUN MAKE-SUBST (pat binding-list)
  (cond ((var-p pat) (establish-replacement pat binding-list))
        ((atom pat) pat)
        (t (cons (make-subst (car pat) binding-list) 
                 (make-subst (cdr pat) binding-list)))))


;;; ESTABLISH-REPLACEMENT gets the current binding of the variable.
;;;  If its unbound, then return the variable.
;;;  If the binding is a list, call make-subst on it.
;;;  If the binding is a variable, call establish-replacement on the result.
;;;  Otherwise return the binding.

(DEFUN ESTABLISH-REPLACEMENT (variable bindings)
  (let ((binding (var-binding variable bindings)))
    (cond ((null binding) variable)
          ((consp binding) (make-subst binding bindings))
          ((var-p binding) (establish-replacement binding bindings))
          (t binding))))



;;;  FORMULA-EQUAL tests for equivalance between statements.  Statements
;;;  are equal if they differ only in the names of their variables.
;;;  We can test for this by unifying the statements, then making sure that
;;;  the resulting binding lists do not constrain any variable in either
;;;  expression.  A variable is "constrained" by a set of bindings if
;;;  it is bound to a non-variable or if it is bound to another variable
;;;  in the same expression (because such a binding means that the two
;;;  variables must be equal). 

(DEFUN FORMULA-EQUAL (form1 form2)
  (let ((bindings (unify form1 form2)))
    (and bindings
         (unconstraining-bindings (formula-vars form1) bindings)
         (unconstraining-bindings (formula-vars form2) bindings))))

;;;   A variable is "constrained" by a set of bindings if
;;;  it is bound to a non-variable or if it is bound to another variable
;;;  in the same expression (because such a binding means that the two
;;;  variables must be equal). This function returns true if none
;;;  of the given variables is constrained by the given bindings. 
        

(DEFUN UNCONSTRAINING-BINDINGS (vars bindings)
  (every #'(lambda (var)
             (let ((binding (make-subst var bindings)))
               (and (var-p binding)
                    (or (eq binding var)
                        (not (member binding vars))))))
         vars))


;;;  SUBSUMES establishes that one formula subsumes the other.
;;;  A formula A subsumes a formula B if they are equivalent or if
;;;  A can be made equivalent to B by binding variables in A.  Intuitively
;;;  this means that A is a generalization of B.  If A subsumes B then A
;;;  necessarily implies B, regardless of what inference rules are present
;;;  in the system. 
;;;
;;;  We use this test to avoid adding a formula to the database if
;;;  it is subsumed by a formula that is already present, e.g.,
;;;  so that we will not add (-> (on block1 block2) (not (clear block2)))
;;;  if (-> (on ?a ?b) (not (clear ?b))) is already there.

;;;  A subsumes B if unification succeeds without constraining
;;;  any variables in B.  If this is true then B will be FORMULA-EQUAL
;;;  to the result of substituting tghe unification bindings into B.

(DEFUN SUBSUMES (form1 form2)
 (let ((bindings (unify form1 form2)))
    (cond ((null bindings) nil)
          ((formula-equal form2 (make-subst form2 bindings))))))
 
