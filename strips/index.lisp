;;;  INDEX.LISP

;;;  INDEX
;;;  
;;;  A facility for creating CAR-indexed databases.  Items are indexed
;;;  by list-structured keys.  Equivalent list structure accesses item.  
;;;  Items are held in buckets so that the same structure can be used ;;;  as a key to several items.  Variables (that match any piece of list 
;;;  structure) are allowed both in storage and retrieval.

;;; ------------------------- DATA TYPES ----------------------
;;;      
;;;    INDEX-NODE 
;;;      PATH: <alist>
;;;      ITEMS: list of <thing>


;;;    INDEX-SEQUENCE: flat list of <thing>


;;; ------------------------ FUNCTIONS ------------------------

;;;  DEFINED

;;;   MAKE-DATABASE( ) => <index-node>

;;;   ADD-TO-INDEX( item:<anything> key:<list> database:<index-node>)
;;;           => <irrelevant>

;;;   FETCH-FROM-INDEX( key:<list> database:<index-node> ) 
;;;           => <stream of <anything>>

;;; ------------------------- DATA TYPES ----------------------

;;;  An INDEX-NODE stands for a partial structure in the index tree.

(DEFSTRUCT INDEX-NODE
  paths items)

(DEFUN INDEX-NODE-SUCCESSOR (index-node index-key)
  (cdr (assoc index-key (index-node-paths index-node))))

(DEFUN ADD-INDEX-NODE-SUCCESSOR (index-node index-key)
  (let ((successor (make-index-node)))
    (setf (index-node-paths index-node)
          (cons (cons index-key successor) 
                   (index-node-paths index-node)))
    successor))

;;;

(defun map-index (fn index-node)
  (for (item :in (index-contents index-node))
       :save (funcall fn item)))

(defun index-contents (index-node)
  (let ((successors (for (item :in (index-node-paths index-node))
                                     :when (cdr item)
                                     :save (cdr item))))
    (append (copy-list (index-node-items index-node))
            (for (successor :in successors)
                 :splice (index-contents successor)))))

;;;  AN INDEX-SEQUENCE is a processed form of the key list that is 
;;;  used to build the index structure.

(DEFUN MAKE-INDEX-SEQUENCE (index-key)
  (for (item :in index-key)
       :splice (make-item-sequence item)))

(DEFUN MAKE-ITEM-SEQUENCE (item)
  (cond ((var-p item)
         (list '*var*))
        ((atom item) 
         (list item))
        (t
         (copy-list (append '(*open-list*)
                            (for (element :in item)
                                 :splice (make-item-sequence element))
                            '(*close-list*))))))

  
;;; ------------------------- FUNCTIONS -----------------------


;;;  Define an index structure for a class of data.

(defmacro defindex (name key-fn &key (remove-test 'equal)
                                     (no-add '(lambda (x y) nil)))
  (defindex-expand name key-fn remove-test))

(defun defindex-expand (name key-fn remove-test)
  (let ((index-name (atcat '* name '*))
        (index-fn-name (atcat "INDEX-" name))
        (fetch-fn-name (atcat "FETCH-" name))
        (erase-fn-name (atcat "ERASE-" name))
        (map-name (atcat "MAP-" name "-INDEX"))
        (reset-name (atcat "RESET-" name "-INDEX")))
    `(progn
       (setf ,index-name (make-index-node))

       ; Define function for adding to index tree
       (defun ,index-fn-name (,name)
         (let* ((index-sequence 
                     (make-index-sequence (apply ,key-fn (list ,name))))
                   (previous-items 
                     (fetch-item index-sequence ,index-name))
                   (matching-items 
                     (for (item :in previous-items)
                          :when (apply #',remove-test (list item ,name))
                          :save item)))
           (for (item :in matching-items)
                :do (remove-from-index item 
                                       index-sequence 
                                       ,index-name))
           (index-item ,name index-sequence ,index-name)))

       ;  Define function for fetching from index tree
       (defun ,fetch-fn-name (pattern)
         (let ((index-sequence (make-index-sequence pattern)))
           (fetch-item index-sequence ,index-name)))

       ; Define function for erasing from index tree
       (defun ,erase-fn-name (,name)
         (let* ((index-sequence 
                      (make-index-sequence (apply ,key-fn (list ,name))))
                   (previous-items 
                      (fetch-item index-sequence ,index-name))
                  (matching-items (for (item :in previous-items)
                                                  :when (equal item ,name)
                                                  :save item)))
           (for (item :in matching-items)
                :do (remove-from-index item 
                                                      index-sequence 
                                                      ,index-name))))

       (defun ,map-name (fn) (map-index fn ,index-name))
         
       (defun ,reset-name () (setf ,index-name (make-index-node))))))



;;;;

(DEFUN INDEX-ITEM (item index-keys index-node)
  (for (key :in (make-index-sequence index-keys))
       :do (setf index-node (or (index-node-successor index-node key)
                                (add-index-node-successor index-node key))))
  (push item (index-node-items index-node))
  item)

(DEFUN REMOVE-FROM-INDEX (item index-keys index-node)
  (for (key :in (make-index-sequence index-keys))
       :when (not (null index-node))
       :do (setf index-node (index-node-successor index-node key)))
  (if (not (null index-node))
      (setf (index-node-items index-node)
            (remove item (index-node-items index-node)))))  

;;; Fetch an item from a data-base.

(DEFUN FETCH-ITEM (index-keys index-node)
   (cond ((null index-node) nil)
         ((null index-keys)
          (copy-list (index-node-items index-node)))
         ((eq (car index-keys) '*var*)
          (union (for (path-pair :in (index-node-paths index-node))
                      :splice
                      (if (eq (car path-pair) '*open-list*)
                          (fetch-with-open-list (cdr index-keys) 
                                                              (cdr path-pair) 0)
                          (fetch-item (cdr index-keys) (cdr path-pair))))
                 nil))
	((eq (car index-keys) '*open-list*)
	 (union (fetch-item (pop-seq-list index-keys)
                                         (index-node-successor index-node '*var*))
                     (fetch-item (cdr index-keys)
                                         (index-node-successor index-node 
                                                                              '*open-list*))))
         (t
          (union (fetch-item (cdr index-keys) 
                                        (index-node-successor index-node 
                                                                            (car index-keys)))
                 (fetch-item (cdr index-keys)
                                    (index-node-successor index-node '*var*))))))
         

(DEFUN FETCH-WITH-OPEN-LIST (index-keys index-node level)
  (for (path-pair :in (index-node-paths index-node))
       :splice
         (case (car path-pair)
           (*open-list* (fetch-with-open-list index-keys 
                                                                   (cdr path-pair) 
                                                                   (1+ level)))
           (*close-list* (if (= level 0)
                             (fetch-item index-keys (cdr path-pair))
                             (fetch-with-open-list index-keys (cdr path-pair) (1- level))))
           (t (fetch-with-open-list index-keys (cdr path-pair) level)))))

;;;  POP-SEQ-LIST removes token corresponding to a list from the
;;;  beginning of the index sequence.  That is, if the first token 
;;;  is  an open list, tokens are removed until the matching close 
;;;  list is popped off.

(DEFUN POP-SEQ-LIST (index-sequence &optional (level 0))
  (if index-sequence
       (case (car index-sequence)
	   (*open-list* (pop-seq-list (cdr index-sequence)
				                   (1+ level)))
	   (*close-list* (if (> level 1)
			     (pop-seq-list (cdr index-sequence)
					         (1- level))
			     (cdr index-sequence)))
	   (t (pop-seq-list (cdr index-sequence) level)))))


;;;  Utility function

(defun atcat (&rest items)
  (let ((result (string (pop items))))
    (for (item :in items)
         :do (setf result (concatenate 'string result (string item))))
    (intern result)))


