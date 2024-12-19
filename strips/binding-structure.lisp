
(defun make-binding-structure ()
   (make-hash-table :size 1000 :rehash-size 2))

(defun binding-structure-add (binding structure)
  (setf (gethash (car binding) structure) (cadr binding)))

(defun binding-structure-get (var structure)
  (gethash var structure))

(defun binding-structure-delete (binding structure)
  (remhash (car binding) structure))

;(defun instantiate (formula structure)
;  (cond ((var-p formula)
;	 (let ((val (binding-structure-get (var-name formula) structure)))
;	   (if val (instantiate val structure) formula)))
;	((atom formula)
;	 formula)
;	((listp formula)
;	 (cons (instantiate (car formula) structure)
;	       (instantiate (cdr formula) structure)))))
	
