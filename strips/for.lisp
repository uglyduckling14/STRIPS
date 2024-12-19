(defmacro for (&rest for-clauses) 
 (let ((var-forms (for-var-forms for-clauses))
          (when-part (member ':when for-clauses))
          (body-forms (for-body for-clauses)))
  (if (and var-forms body-forms)
        (for-expander var-forms when-part body-forms)
        (error "Bad FOR syntax: ~S" (cons 'for for-clauses)))))

;;; for-var-forms gets the (variable :in list) forms

(defun for-var-forms (l) 
 (and l (listp (car l)) (cons (car l) (for-var-forms (cdr l))))) 

;;; for-body returns (:do form) or (:save form)

(defun for-body (l) 
 (and l (or (and (member (car l) '(:always :do :first :save :splice))
                 l)
            (for-body (cdr l))))) 

;;; for-expander returns (mapping-function lambda-form lists)

(defun for-expander (var-forms when-part body-forms)
 (let ((keyword (car body-forms)) 
       (body `(progn ,@(cdr body-forms))) 
       (vars (mapcar #'car var-forms)) 
       (lists (mapcar #'(lambda (var-form) (caddr var-form)) 
                              var-forms))) 
  `(,(for-mapfn keyword when-part) 
    #'(lambda ,vars ,(for-mapfn-body keyword when-part body)) 
   ,@lists))) 

;;; for-mapfn returns the name of a mapping function

(defun for-mapfn (keyword when-part) 
 (case keyword 
    ((:always) 'every)
    ((:do) 'mapc)
    ((:first) 'some)
    ((:save) (if when-part 'mapcan 'mapcar))
    ((:splice) 'mapcan)
    (t (error "Unknown FOR keyword ~S" keyword))))

;;; for-mapfn-body adds the when-form to the body, if necessary

(defun for-mapfn-body (keyword when-part body)
 (case keyword 
    ((:always) (if when-part
                   `(or (not ,(cadr when-part)) ,body)
                   body))
    ((:do :first) (if when-part
                      `(and ,(cadr when-part) ,body)
                      body))
    ((:save)  (if when-part
                  `(and ,(cadr when-part) (list ,body))
                  body))
    ((:splice) `(copy-list
                 ,(if when-part
                      `(and ,(cadr when-part) ,body)
                      body)))
    (t (error "Unknown FOR keyword ~S" keyword))))
