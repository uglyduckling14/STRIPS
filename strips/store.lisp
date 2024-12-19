;;; STORE.lisp

;;;
;;;  STORE
;;;  
;;;  A facility for storing facts in a database, and
;;;  forward-chaining from these facts.  This prodides the addition 
;;;  of datum structures and the marking of data as IN or OUT.

;;; -----------------  DATA STRUCTURES --------------------------

;;;   DATUM:
;;;      FORMULA <formula>
;;;      STATUS IN|OUT

;;; ----------------- FUNCTIONS -------------------------------


;;;   DEFINED

;;;     STORE( formula: <formula> ) => <datum>

;;;     LOOKUP( formula: <formula> ) => <datum>

;;; -----------------  DATA STRUCTURES -------------------------

;;; Formulas are stored in the dabase as structures called 
;;; DATUMs.  These allow other information to be associated with the 
;;; formula that is stored.  For example, the STATUS field allows us 
;;; to mark a stored formula OUT when it is RETRACTED.  A formula 
;;; marked OUT is treated as though it were not in the database at all.  
;;; Leaving the datum in place saves time if the formula is 
;;; reasserted, since all we have to do is change the status back to 
;;; IN.  Later on, we will add other information to DATUMS, in 
;;; particular markings that allow us to determine why a formula 
;;; was added to the database in the first place.

;;; ----------------- FUNCTIONS ----------------------------

;;;  STORE adds a new fact to the database, unless an equivalent 
;;;  fact is already there. 
;;;  Facts are put into DATUM structures and added to the data base.
;;;  If they are already in the data-base, they are not added.  When a
;;;  fact is added any forward chaining rules that match it are run.
;;;  When a new rule is STOREed, all inferences allowed to it by the 
;;;  facts in the data base are made.

;;; STORE prints out a message if *quiet-db* is nil)

(DEFUN STORE (formula &optional support)
  (setf formula (uniquify formula))
  (if (fc-rule-p formula) (setf formula (normalize-fc-rule formula)))
  (let ((old-datum (car (fetch-fact formula))))
     (cond ((and old-datum (subsumes (datum-formula old-datum) formula))
                (already-stored-msg (datum-formula old-datum))
                old-datum)
              (t
                (let ((new-datum (index-store-input formula)))  
                   (store-msg formula)
                   (infer-from-data new-datum)
                    new-datum)))))

;;;  INDEX-STORE-INPUT adds a new fact or rule to the data base.
;;;  Rules are added both to the appropriate rule data base (from
;;;  which they can be retrieved by input facts or queries that
;;;  match them), and to the fact data base.  The latter is so that
;;;  when we are given a new rule to add, we can look it up and see if
;;;  it is already there.

(DEFUN INDEX-STORE-INPUT (formula)
  (let ((datum (make-datum :formula formula 
                                            :status 'in)))
    (if (fc-rule-p formula)
         (index-fc-rule datum))
    (if (bc-rule-p formula)
         (index-bc-rule datum))
    (index-fact datum)
     datum))

;;; INFER-FROM-DATA takes a fact or a rule being added to the
;;; data base and makes the appropriate inferences from it.
;;;  New facts are matched against the ANTEs of all appropriate
;;;  foward chaining rules already in the data-base.
;;;  New forward chaining rules are matched (or their ANTEs are 
;;;  matched) against facts already in the data-base.
;;;  Backward chaining rules do not provide any new inferences.
 
(DEFUN INFER-FROM-DATA (datum)
  (let ((fact (datum-formula datum)))
    (cond ((fc-rule-p fact)
               (make-fc-inferences (list datum) 
                                                 (fetch-fact (fc-rule-ante fact))))
              ((bc-rule-p fact) 
                nil)
              (t 
                (make-fc-inferences (fetch-fc-rule fact) (list datum))))))


;;;  MAKE-FC-INFERENCES takes two lists, rules and facts.
;;;  For each formula in the fact list, it tries to unify the formula
;;;  with the ANTE of each of the rules.  When it succeeds in unifying,
;;;  it substitutes the variables in the CONSE with those in the 
;;;  binding list and STOREs the resulting statement.

(DEFUN MAKE-FC-INFERENCES (rule-list data-list)
   (for (datum :in data-list)
      :do (for (rule :in rule-list)
              :do (fc-infer datum rule))))
  
(DEFUN FC-INFER (datum rule)
  (if (and (in-p datum)
               (in-p rule))
       (let ((bindings (unify (datum-formula datum) 
                                         (fc-rule-ante (datum-formula rule)))))
          (if bindings
               (store (make-subst (fc-rule-conse (datum-formula rule))          
                                               bindings))))))

;;; OUTPUT functions

(setf *quiet-db* nil)

(defun STORE-MSG (formula)
  (if (not *quiet-db*)
    (format t "  STOREing => ~A~%"  formula)))

(defun ALREADY-STORED-MSG (formula)
  (if (not *quiet-db*)
    (format t "~A already in DB.~%" formula)))

(defun RETRACT-MSG (formula)
  (if (not *quiet-db*)
    (format t "  RETRACTing => ~A~%"  formula)))


