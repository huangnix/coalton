(coalton-library/utils:defstdlib-package :coalton-library/ordtree2
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/hash
   #:coalton-library/tuple
   #:coalton-library/functions)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:cell #:coalton-library/cell))
  (:shadow #:empty)
  (:export
   #:Tree #:empty
   #:empty?
   #:lookup
   #:insert
   #:replace
   #:remove
   #:update))

(in-package :coalton-library/ordtree2)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;; Based on Ralf Hinze, Purely Functional 1-2 Brother Trees
;; https://www.researchgate.net/publication/220676591_Purely_Functional_1-2_Brother_Trees

(cl:defmacro match-fn (cl:&body clauses)
  (cl:let ((arg (cl:gensym)))
    `(fn (,arg)
       (match ,arg
         ,@clauses))))

(cl:defmacro match-define (name cl:&body clauses)
  `(define ,name (match-fn ,@clauses)))

(coalton-toplevel
  (define-type (Tree :elt)
    "A 1-2 brother tree, sorted by `<=>` and unique by `==`."

    Empty
    "exported; an empty tree."

    (N1 (Tree :elt))
    "unexported; unary node"

    (N2 (Tree :elt) :elt (Tree :elt))
    "unexported; binary node"

    (N3 (Tree :elt) :elt (Tree :elt) :elt (Tree :elt))
    "unexported; ternary node - only appear intermediately during balancing"

    (L2 :elt)
    "unexported; leaf node - only appear intermediately during balancing"
    )

  (declare empty? (Tree :elt -> Boolean))
  (define (empty? t)
    (match t
      ((Empty) True)
      (_ False)))

  ;;; aux
  (declare stray-node (Unit -> :a))
  (define (stray-node)
    (error "Implementation error: Encountered ephemeral node during traversal"))

  ;;; searching trees

  (declare lookup (Ord :elt => Tree :elt -> :elt -> Optional :elt))
  (define (lookup haystack needle)
    "If HAYSTACK contains an element `==` to NEEDLE, return it."
    (match haystack
      ((Empty) None)
      ((N1 t) (lookup t needle))
      ((N2 left elt right)
       (match (<=> needle elt)
         ((LT) (lookup left needle))
         ((EQ) (Some elt))
         ((GT) (lookup right needle))))
      (_ (stray-node))))

  ;; smart constructors; eliminating intermediate node
  (declare make-root (Tree :elt -> Tree :elt))
  (inline)
  (define (make-root t)
    (match t
      ((L2 a) (N2 Empty a Empty))
      ((N3 t1 a1 t2 a2 t3) (N2 (N2 t1 a1 t2) a2 (N1 t3)))
      (_ t)))

  (declare make-n1 (Tree :elt -> Tree :elt))
  (inline)
  (define (make-n1 t)
    (match t
      ((L2 a) (N2 Empty a Empty))
      ((N3 t1 a1 t2 a2 t3) (N2 (N2 t1 a1 t2) a2 (N1 t3)))
      (_ (N1 t))))

  (declare make-n2 (Tree :elt -> :elt -> Tree :elt -> Tree :elt))
  (inline)
  (define (make-n2 tl a tr)
    (match tl
      ((L2 a1) (N3 Empty a1 Empty a tr))
      ((N3 t1 a1 t2 a2 t3)
       (match tr
         ((N1 t4) (N2 (N2 t1 a1 t2) a2 (N2 t3 a t4)))
         (_       (N3 (N2 t1 a1 t2) a2 (N1 t3) a tr))))
      ((N1 t1)
       (match tr
         ((N3 t2 a2 t3 a3 t4) (N2 (N2 t1 a t2) a2 (N2 t3 a3 t4)))
         ((L2 a2)             (N3 tl a Empty a2 Empty))
         (_                   (N2 tl a tr))))
      ((N2 _ _ _)
       (match tr
         ((N3 t2 a2 t3 a3 t4) (N3 tl a (N1 t2) a2 (N2 t3 a3 t4)))
         ((L2 a2)             (N3 tl a Empty a2 Empty))
         (_                   (N2 tl a tr))))
      (_
       (match tr
         ((L2 a2)             (N3 tl a Empty a2 Empty))
         (_                   (N2 tl a tr))))))


  (declare insert (Ord :elt => Tree :elt -> :elt -> Tree :elt))
  (define (insert t a)
    (let ((ins (fn (n)
                 (match n
                   ((Empty)    (L2 a))
                   ((N1 t1)    (make-n1 (ins t1)))
                   ((N2 l b r)
                    (match (<=> a b)
                      ((LT)    (make-n2 (ins l) b r))
                      ((EQ)    (N2 l a r))
                      ((GT)    (make-n2 l b (ins r)))))
                   (_ (stray-node))))))
      (make-root (ins t))))

  (declare replace (Ord :elt => Tree :elt -> :elt -> Tree :elt))
  (define (replace t a)
    (let ((rep (fn (n)
                 (match n
                   ((Empty)    (L2 a))
                   ((N1 t1)    (make-n1 (rep t1)))
                   ((N2 l b r)
                    (match (<=> a b)
                      ((LT)    (make-n2 (rep l) b r))
                      ((EQ)    (N2 l b r))
                      ((GT)    (make-n2 l b (rep r)))))
                   (_ (stray-node))))))
      (make-root (rep t))))



  )
