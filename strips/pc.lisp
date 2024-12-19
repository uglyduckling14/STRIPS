;;; pc.lisp

;;;  PC
;;;  
;;; Functions to test and manipulate PC formula.
;;;   

(DEFUN QMARK-READER (stream char)
  (make-var :name (read stream t t)))

(SET-MACRO-CHARACTER  #\? #'qmark-reader t)


(DEFSTRUCT (VAR (:print-function print-var))
  name)

(DEFUN PRINT-VAR (var stream level)
  (princ "?" stream)
  (princ (var-name var) stream))
						
(DEFUN VAR-EQUAL (var1 var2)
  (and (var-p var1)
       (var-p var2)
       (equal (var-name var1) (var-name var2))))



;;; ------------------------ DATA TYPES --------------------------------------

;;;  DEFINED


;;;    FC-RULE: (-> antecedent: <formula> consequent: <formula> )
;;;    BC-RULE: (<- consequent: <formula> antecedent: <formula> )

;;;    Formula - a wff.

;;;  Forward-chaining rules have the form (-> <antecedent> <consequent>).
;;;  They are stored in the database *FC-RULES* as facts, though 
;;;  they are handled specially by ASSERT.  When a fact is ASSERTed 
;;;  that unifies with the antecedent of an fc-rule, the consequent
;;;  is ASSERTed with substitutions from the unification result.

;;;  Backward--chaining rules have the form (-> <antecedent> <consequent>).
;;;  They are stored in the database *BC-RULES* as facts, though 
;;;  they are handled specially by ASSERT.  

(DEFUN FORMULA-VARS (formula)
  (cond ((var-p formula)
         (list formula))
        ((atom formula)
         nil)
        (t
         (union (formula-vars (car formula))
                (formula-vars (cdr formula))))))

(DEFUN FC-RULE-P (pat)
  (and (listp pat)
       (eq (car pat) '->)))

(DEFUN FC-RULE-ANTE (rule)
  (if (listp rule)
      (cadr rule)))

(DEFUN FC-RULE-CONSE (rule)
  (if (listp rule)
      (caddr rule)))


(DEFUN BC-RULE-P (pat)
  (and (listp pat)
       (eq (car pat) '<-)))

(DEFUN BC-RULE-ANTE (rule)
  (if (listp rule)
      (caddr rule)))

(DEFUN BC-RULE-CONSE (rule)
  (if (listp rule)
      (cadr rule)))

(DEFUN PREDICATE (formula)
  (if (listp formula)
      (car formula)))

;;;  A forward-chaining rule with a conjunctive antecedent, 
;;;  e.g. (-> (AND A B C) D), is interpreted as a set of 
;;;  embedded rules, e.g. (-> A (-> B (-> C D))).  This 
;;;  function makes this transformation.
;;;
;;;  This actually has an interesting consequence for 
;;;  dependency structures.  It means that what the user 
;;;  sees as a single justification structure, e.g., A B C and 
;;;  the rule support D, is actually stored as a chain of 
;;;  structures, e.g. A and the transformed rule support 
;;;  (-> B (-> C D)); this rule and B support (-> C D). This 
;;;  it the way the user thinks about it.  However, the 
;;;  structure  can be read out again in the rule and C support 
;;;  D.  It would be a mess to try to store form that the user
;;;  visualizes; thus, the way to avoid uninuitiveness is
;;;  probably to make functions that display the justification
;;;  structures sensitive to this.  


(DEFUN NORMALIZE-FC-RULE (rule)
 (if (conjunctive-p (fc-rule-ante rule))
   (normalize-fc-test (cdr (fc-rule-ante rule)) (fc-rule-conse rule))
   rule))

(DEFUN NORMALIZE-FC-TEST (lhs rhs)
  (if (null lhs)
    rhs
    (list '-> (car lhs) (normalize-fc-test (cdr lhs) rhs))))


;;; UNIQUIFY takes a formula and renames all variables with unique new names.
;;; If there are no variables in the pattern it is not copied.

(DEFUN UNIQUIFY (pattern)
  (let ((new-names (rename-list pattern nil)))
    (if (null new-names)
      pattern
      (rename-variables pattern new-names))))


;;; RENAME-LIST takes a pattern and a list of new names.  It returns a list 
;;; of lists of the form (old-name new-name).  New-name is a copy of old-name
;;; that is not in LISPs symbol list.

(DEFUN RENAME-LIST (pattern &optional new-names)
  (cond ((var-p pattern)
         (let ((name (var-name pattern)))
           (cond ((assoc name new-names) new-names)
                 (t (cons 
                     (list name (make-var :name (copy-symbol name))) 
                     new-names)))))
        ((consp pattern)
         (rename-list (car pattern)
                      (rename-list (cdr pattern) new-names)))
        (t new-names)))



;;; RENAME-VARIABLES is handed a pattern and a list of new-names.  For every 
;;; original variable it replaces the existing variable structure with the 
;;; new one.

(DEFUN RENAME-VARIABLES (pattern new-names)
  (cond ((var-p pattern)
         (let ((entry (assoc (var-name pattern) new-names)))
           (if entry (cadr entry) pattern)))
        ((atom pattern) pattern)
        (t (cons (rename-variables (car pattern) new-names) 
                 (rename-variables (cdr pattern) new-names)))))


;;; Test to see is a formula is a conjunct.

(DEFUN CONJUNCTIVE-P (formula)
  (and (listp formula)
       (eq (car formula) 'and)))


;;; Test to see if two formulas are indepenent

(DEFUN INDEPENDENT (form1 form2)
  (null (intersection (collect-variables form1) (collect-variables form2))))

;;; Collect all of the variable names from a formula

(DEFUN COLLECT-VARIABLES (form)
  (cond ((var-p form) (list (var-name form)))
        ((atom form) nil)
        (t (for (item :in form)
                :splice (collect-variables item)))))

(defun no-vars-in (form)
  (null (collect-variables form)))

