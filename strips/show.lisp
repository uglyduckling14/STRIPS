

(in-package "USER")

(defvar *show-level* 0)
(defvar *seen-by-show* nil)
(defvar *show-depth-limit* 0)
(defvar *default-depth-limit* 7)

(defun show* (query)
   (let ((result (show1 query)))
      (if result (make-subst query result))))

(defun SHOW (query &optional (depth-limit *default-depth-limit*))
   (setf *seen-by-show* nil)
   (setf *show-level* 0)
   (setf *show-depth-limit* depth-limit)
   (show0 query))

(defun SHOW1 (query)
   (let ((result nil)
           (iteration-limit 50)
           (iteration-increment 3))
      (dotimes (iter iteration-limit)
          (if (setf result (show query (* iter iteration-increment)))
               (return (car result)))))) 

(DEFUN SHOW0 (query)
   (setf *show-level* (1+ *show-level*))
   ;(format t "SHOW (~A): " *show-level*)
   ;(pp query)
   ;(format t "~%")
   (let ((result nil))
     (when 
;          (and 
      (<= *show-level* *show-depth-limit*)
;		(not (for (s-pair :in *seen-by-show*)
;			  :first (and (not (cadr s-pair))
;				      (formula-equal (car s-pair) query)))))
         (push (list query nil) *seen-by-show*)
         (if (conjunctive-p query) 
              (setf result (show-conj (cdr query)))
              (setf result (union (show-direct query)
                                            (show-bc query))))
         (pop *seen-by-show*))
      (setf *show-level* (1- *show-level*))
      result))

(DEFUN SHOW-DIRECT (query)
   (let ((bindings nil))
      (for (fact :in (fetch-fact query))
          :when (and (in-p fact)
		     (setf bindings (unify query (datum-formula fact))))
          :save bindings)))

(DEFUN SHOW-BC (query)
  (for (rule-datum :in (fetch-bc-rule query))
     :when (in-p rule-datum)
     :splice (let* ((rule (uniquify (datum-formula rule-datum)))
                          (bindings (unify query (bc-rule-conse rule))))
                   (if bindings
		       (let ((bc-results (show-bc-ante query rule bindings)))
                        (for (new-bind :in bc-results)
                              :save (union new-bind bindings)))))))


(defun show-bc-ante (query rule bindings)
  (if (constraining-p bindings query)
      (setf (cadr (car *seen-by-show*)) t))
  (prog1 (show0 (make-subst (bc-rule-ante rule) bindings))
         (setf (cadr (car *seen-by-show*)) nil)))

(defun constraining-p (bindings formula)
  (not (equal (make-subst formula bindings) formula)))



(defun SHOW-CONJ (conjuncts)
   (let* ((answer-stream (show0 (car conjuncts))))
      (for (conj :in (cdr conjuncts))
          :do (for (binds :in answer-stream)
                   :do (let* ((new-binds (show0 (make-subst conj binds))))
                           (if (null new-binds)
                                (setf answer-stream 
                                         (remove binds answer-stream))
                                (setf answer-stream
                                         (append 
                                            (for (nbinds :in new-binds)
                                                :save (union nbinds binds))
                                            (remove binds answer-stream)))))))
        answer-stream))



