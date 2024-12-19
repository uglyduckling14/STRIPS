

(DEFSTRUCT DATUM 
  formula
  status
  supporters
  i-support)

(defun print-datum (datum stream level)
  (format stream "#S(DATUM FORMULA ~A STATUS ~A)" 
	(datum-formula datum) (datum-status datum)))

(DEFUN IN-P (datum)
  (eq (datum-status datum) 'in))

(DEFINDEX fc-rule 
#'(lambda (item) (fc-rule-ante (datum-formula item)))
  :no-add (lambda (item1 item2) 
            (formula-equal (datum-formula item1) 
                           (datum-formula item2))))

(DEFINDEX bc-rule 
#'(lambda (item) (bc-rule-conse (datum-formula item)))
  :no-add (lambda (item1 item2) 
            (formula-equal (datum-formula item1) 
                           (datum-formula item2))))

(DEFINDEX fact 
#'(lambda (item) (datum-formula item))
  :no-add (lambda (item1 item2) 
            (formula-equal (datum-formula item1) 
                           (datum-formula item2))))


(DEFUN RESET-DB ()
  (reset-fact-index)
  (reset-fc-rule-index)
  (reset-bc-rule-index))

          
(DEFUN LOOKUP (formula)
  (find-if 
  #'(lambda (datum) (formula-equal (datum-formula datum) formula))
           (cond ((bc-rule-p formula)
                  (fetch-bc-rule formula))
                 ((fc-rule-p formula)
                  (fetch-fc-rule formula))
                 (t
                  (fetch-fact formula)))))

(DEFUN LOOKUP-SUBSUMES (formula)
  (find-if #'(lambda (datum) (subsumes (datum-formula datum) formula))
           (cond ((bc-rule-p formula)
                  (fetch-bc-rule formula))
                 ((fc-rule-p formula)
                  (fetch-fc-rule formula))
                 (t
                  (fetch-fact formula)))))




(reset-db)
