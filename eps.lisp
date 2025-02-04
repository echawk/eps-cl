
;; An implementation of Expansion-Passing Style in Common Lisp.

;; See: https://link.springer.com/article/10.1007/BF01806176

(defpackage :eps (:use :cl))

(in-package :eps)

(setq expanders '())

(defun expanderp (symbol)
  (assoc symbol expanders))

(defun expander-function (symbol)
  (cdr (assoc symbol expanders)))

(defun install-expander (symbol expander-fn)
  (setq expanders (cons (cons symbol expander-fn) expanders)))

(defun initial-expander (form expander)
  (cond
    ((or (atom form) (symbolp form) (consp form)) form)
    ((expanderp (car form))
     ((expander-function (car form)) form expander))
    (t (mapcar (lambda (form) (expander form expander)) form))))


(trace initial-expander)

(defun expand (form)
  (initial-expander form #'initial-expander))

(install-expander
 'quote
 (lambda (x e)
   x))

(install-expander
 'lambda
 (lambda (x e)
   `(lambda ,(cadr x)
      ,@(mapcar (lambda (x) (apply #'e x e)) (cddr x)))))

(install-expander
 'if
 (lambda (x e)
   `(if ,@(mapcar (lambda (x) (apply #'e x e)) (cdr x)))))

;; CL style macro system in EPS style.

(defun destructure (pattern access bindings)
  (cond
    ((null    pattern) bindings)
    ((symbolp pattern) (cons `(,pattern ,access) bindings))
    ((consp   pattern)
     (destructure (car pattern) `(car ,access)
                  (destructure (cdr pattern) `(cdr ,access)
                               bindings)))))

(defun make-macro (pattern body)
  (let ((x (gensym))
        (e (gensym)))
    `(lambda (x e)
       (apply #',e (let ,(destructure pattern `(cdr ,x) '())
                     ,body)
              #',e))))


(install-expander
 'defmacro
 (lambda (x e)
   (let ((keyword (cadr   x))
         (pattern (caddr  x))

         (body    (cadddr x)))

     (apply
      #'e `(install-expander
            ',keyword
            ,(make-macro pattern body))
      #'e))))

;; (expand '(defmacro let (decls . body)
;;           `((lambda ,(mapcar car decls) ,@body)
;;             ,@ (map cadr decls))))


(expand '(if x (quote 1) 2))
