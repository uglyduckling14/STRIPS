
#|
================================================================
module: loader.lisp
description: Loads an ANSI Common Lisp Implementation of STRIPS
bugs to vladimir kulyukin in Canvas
================================================================
|#

(in-package "USER")

(defparameter *strips-files*
  '("for.lisp"
    "pc.lisp"
    "index.lisp"
    "unify.lisp"
    "database.lisp"
    "store.lisp"
    "show.lisp"
    "binding-structure.lisp"
    "srules.lisp"
    "strips.lisp"
    ))

(defun load-strips ()
  (dolist (file *strips-files* t)
    (load file :verbose t :print t)))

;;; end-of-file

