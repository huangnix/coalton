(coalton-library/utils:defstdlib-package :coalton-library/ordtree2
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/hash
   #:coalton-library/tuple
   #:coalton-library/functions
   #:coalton-library/math)
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


  (declare consistent? (Tree :elt -> Boolean))
  (define (consistent? t)
    "Check invariance condition of the tree `t`.  If the condition is broken,
an error is thrown."
    (let ((dep (fn (t)
                 "Returns the depth of nullary tree, while checking other conditions."
                 (match t
                   ((Empty) 0)
                   ((N1 (N1 _)) (error "Unary node has unary child"))
                   ((N1 t) (1+ (dep t)))
                   ((N2 l _ r) (let ((dl (dep l))
                                     (dr (dep r)))
                                 (if (== dl dr)
                                     (1+ dl)
                                     (error "Depth inbalance"))))
                   (_ (error "Ephemeral node"))))))
      (dep t)
      True))

  ;;; searching
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
      ((N1 t) t)
      (_ t)))

  (declare make-n1 (Tree :elt -> Tree :elt))
  (inline)
  (define (make-n1 t)
    (match t
      ((L2 a) (N2 Empty a Empty))
      ((N3 t1 a1 t2 a2 t3) (N2 (N2 t1 a1 t2) a2 (N1 t3)))
      (_ (N1 t))))

  (declare make-n2i (Tree :elt -> :elt -> Tree :elt -> Tree :elt))
  (inline)
  (define (make-n2i tl a tr)
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
         ((N1 t2)             (N1 (N2 t1 a t2)))
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

  (declare make-n2 (Tree :elt -> :elt -> Tree :elt -> Tree :elt))
  (inline)
  (define (make-n2 tl a tr)
    (match tl
      ((L2 a1) (N3 Empty a1 Empty a tr))
      ((N3 t1 a1 t2 a2 t3)
       (match tr
         ((N1 t4) (N2 (N2 t1 a1 t2) a2 (N2 t3 a t4)))
         (_       (N3 (N2 t1 a1 t2) a2 (N1 t3) a tr))))
      ((N1 (N1 t1))
       (match tr
         ((N2 (N1 t2) a2 (= t3 (N2 _ _ _))) (N1 (N2 (N2 t1 a t2) a2 t3)))
         ((N2 (N2 t2 a2 t3) a3 (N1 t4))     (N1 (N2 (N2 t1 a t2) a2
                                                    (N2 t3 a3 t4))))
         ((N2 (= t2 (N2 _ _ _)) a2 (= t3 (N2 _ _ _)))
                                            (N2 (N2 (N1 t1) a t2) a2
                                                (N1 t3)))
         ((N1 t2)                           (N1 (N2 (N1 t1) a t2)))
         (_                                 (N2 tl a tr))))
      ((N1 t1)
       (match tr
         ((N3 t2 a2 t3 a3 t4) (N2 (N2 t1 a t2) a2 (N2 t3 a3 t4)))
         ((L2 a2)             (N3 tl a Empty a2 Empty))
         ((N1 t2)             (N1 (N2 t1 a t2)))
         (_                   (N2 tl a tr))))
      ((N2 (N1 t1) a1 (N2 t2 a2 t3))
       (match tr
         ((N1 (N1 t4))        (N1 (N2 (N2 t1 a1 t2) a2
                                      (N2 t3 a t4))))
         ((N3 t2 a2 t3 a3 t4) (N3 tl a (N1 t2) a2 (N2 t3 a3 t4)))
         ((L2 a2)             (N3 tl a Empty a2 Empty))
         (_                   (N2 tl a tr))))
      ((N2 (= t1 (N2 _ _ _)) a1 (N1 t2))
       (match tr
         ((N1 (N1 t3))        (N1 (N2 t1 a1 (N2 t2 a t3))))
         ((N3 t2 a2 t3 a3 t4) (N3 tl a (N1 t2) a2 (N2 t3 a3 t4)))
         ((L2 a2)             (N3 tl a Empty a2 Empty))
         (_                   (N2 tl a tr))))
      ((N2 (= t1 (N2 _ _ _)) a1 (= t2 (N2 _ _ _)))
       (match tr
         ((N1 (= t3 (N1 _)))  (N2 (N1 t1) a1 (N2 t2 a t3)))
         ((N3 t2 a2 t3 a3 t4) (N3 tl a (N1 t2) a2 (N2 t3 a3 t4)))
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
                      ((LT)    (make-n2i (ins l) b r))
                      ((EQ)    (N2 l a r))
                      ((GT)    (make-n2i l b (ins r)))))
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
                      ((LT)    (make-n2i (rep l) b r))
                      ((EQ)    (N2 l b r))
                      ((GT)    (make-n2i l b (rep r)))))
                   (_ (stray-node))))))
      (make-root (rep t))))

  (declare remove (Ord :elt => Tree :elt -> :elt -> Tree :elt))
  (define (remove t a)
    (let ((del (fn (n)
                 (match n
                   ((Empty) Empty)
                   ((N1 t)  (N1 (del t)))
                   ((N2 l b r)
                    (match (<=> a b)
                      ((LT) (make-n2 (del l) b r))
                      ((EQ) (match (split-min r)
                              ((None)               (N1 l))
                              ((Some (Tuple a1 r1)) (make-n2 l a1 r1))))
                      ((GT) (make-n2 l b (del r)))))
                   (_ (stray-node))))))
      (make-root (del t))))

  (define (split-min n)
    "Helper for removal"
    (match n
      ((Empty)       None)
      ((N1 t)        (match (split-min t)
                       ((None)              None)
                       ((Some (Tuple a t1)) (Some (Tuple a (N1 t1))))))
      ((N2 t1 a1 t2) (match (split-min t1)
                       ((None)               (Some (Tuple a1 (N1 t2))))
                       ((Some (Tuple a t11)) (Some (Tuple a
                                                          (make-n2 t11 a1 t2))))))
      (_ (stray-node))))

  (declare update (Ord :elt => Tree :elt -> :elt
                       -> (Optional :elt -> (Tuple (Optional :elt) :a))
                       -> (Tuple (Tree :elt) :a)))
  (define (update t a f)
    (let ((unchanged? (fn (a b)
                        (lisp Boolean (a b)
                          (cl:eq a b))))
          (walk (fn (n)
                  (match n
                    ((Empty)
                     (match (f None)
                       ((Tuple (None) aux) (Tuple n aux))
                       ((Tuple (Some x) aux)
                        (Tuple (L2 x) aux)))) ; insert
                    ((N1 t1)
                     (let (Tuple t2 aux) = (walk t1))
                     (if (unchanged? t1 t2)
                         (Tuple n aux)
                         (Tuple (make-n1 t2) aux)))
                    ((N2 l b r)
                     (match (<=> a b)
                       ((LT) (let (Tuple ll aux) = (walk l))
                             (if (unchanged? l ll)
                                 (Tuple n aux)
                                 (Tuple (make-n2 ll b r) aux)))
                       ((EQ) (match (f (Some b))
                               ((Tuple (None) aux) ; delete
                                (Tuple (N1 l) aux))
                               ((Tuple (Some b2) aux) ;replace
                                ;; NB: `f` must guarantee that the new element
                                ;; is `==` to the old element.  Otherwise
                                ;; the tree silently breaks.
                                (if (unchanged? b b2)
                                    (Tuple n aux)
                                    (Tuple (N2 l b2 r) aux)))))
                       ((GT) (let (Tuple rr aux) = (walk r))
                             (if (unchanged? r rr)
                                 (Tuple n aux)
                                 (Tuple (make-n2 l b rr) aux)))))
                    (_ (stray-node))))))
      (let (Tuple t2 aux) = (walk t))
      (Tuple (make-root t2) aux)))
  )
