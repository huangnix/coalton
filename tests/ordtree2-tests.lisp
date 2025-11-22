(in-package #:coalton-native-tests)

(define-test ordtree2-basic-test ()
  "OrdTree basic operations"
  (let a = (the (ordtree2:OrdTree Integer) ordtree2:empty))

  (is (ordtree2:empty? a))
  (is (== (ordtree2:lookup a 1) None))

  (let b = (ordtree2:insert a 1))
  (is (== (ordtree2:lookup b 1) (Some 1)))

  ;; This data sequence is long enough to cover the branches in
  ;; rebalancing routines.
  (let data = (map (fn (k) (bits:and #xffffffff (* k 2654435761)))
                   (range 0 4096)))

  (let c = (fold ordtree2:insert a data))
  (is (some? (fold (fn (m k) (>>= (ordtree2:lookup c k) Some))
                   (Some 1) data)))
  (is (ordtree2::consistent? c))

  (let d = (fold (fn (m k)
                   (match (ordtree2:lookup m k)
                     ((None) (lisp :a (k)
                               (cl:error "Key ~a dissapeared unexpectedly" k)))
                     ((Some _) Unit))
                   (let ((m1 (ordtree2:remove m k)))
                     (match (ordtree2:lookup m1 k)
                       ((None) Unit)
                       ((Some _) (lisp :a (k)
                                   (cl:error "Key ~a failed to be removed" k))))
                     (if (ordtree2::consistent? m1)
                         Unit
                         (lisp :a (k)
                           (cl:error "Key ~a removal caused inconsistency" k)))
                     m1))
                 c data))
  (is (ordtree2:empty? d))

  (let (Tuple bb prev) = (ordtree2:update a 1 (fn (z) (Tuple (Some 1) z))))
  (is (== (ordtree2:lookup bb 1) (Some 1)))
  (is (== prev None))
  (let (Tuple bb2 prev2) = (ordtree2:update bb 1 (Tuple None)))
  (is (ordtree2:empty? bb2))
  (is (== prev2 (Some 1)))

  (let cc = (fold (fn (m k)
                     (let (Tuple mm _) =
                       (ordtree2:update m k (fn (_) (Tuple (Some k) False))))
                    mm)
                  a data))
  (is (some? (fold (fn (m k) (>>= (ordtree2:lookup cc k) Some))
                   (Some 1) data)))
  (is (ordtree2::consistent? cc))

  (let dd = (fold (fn (m k)
                    (let (Tuple m1 _) = (ordtree2:update m k (Tuple None)))
                    (is (ordtree2::consistent? m1))
                    m1)
                  cc data))
  (is (ordtree2::empty? dd))
  )

(coalton-toplevel
  (define-type OrdTreeTestEntry
    (OrdTreeTestEntry Integer String))
  (define (ordtree-test-entry-key (OrdTreeTestEntry k _)) k)
  (define-instance (Eq OrdTreeTestEntry)
    (define (== a b)
      (== (ordtree-test-entry-key a) (ordtree-test-entry-key b))))
  (define-instance (Ord OrdTreeTestEntry)
    (define (<=> a b)
      (<=> (ordtree-test-entry-key a) (ordtree-test-entry-key b))))
  )

(define-test ordtree2-update-test ()
  "OrdTree update operations with more involved update function"
  (let a = (the (ordtree2:OrdTree OrdTreeTestEntry)
                (fold ordtree2:insert ordtree2:empty
                      (make-list (OrdTreeTestEntry 1 "one")
                                 (OrdTreeTestEntry 2 "two")
                                 (OrdTreeTestEntry 3 "three")
                                 (OrdTreeTestEntry 4 "four")))))

  ;; lookup - hit
  (match (ordtree2:update a (OrdTreeTestEntry 1 "")
                          (fn (e)
                            (match e
                              ((None) (Tuple None "nope"))
                              ((Some (OrdTreeTestEntry _ v)) (Tuple e v)))))
    ((Tuple b v)
     (is (== v "one"))
     (is (== (ordtree2:lookup b (OrdTreeTestEntry 1 ""))
             (Some (OrdTreeTestEntry 1 "one"))))))

  ;; lookup - miss
  (match (ordtree2:update a (OrdTreeTestEntry 0 "")
                          (fn (e)
                            (match e
                              ((None) (Tuple None "nope"))
                              ((Some (OrdTreeTestEntry _ v)) (Tuple e v)))))
    ((Tuple _ v) (is (== v "nope"))))

  ;; delete - hit
  (match (ordtree2:update a (OrdTreeTestEntry 2 "")
                          (fn (e)
                            (match e
                              ((None) (Tuple None "nope"))
                              ((Some (OrdTreeTestEntry _ v)) (Tuple None v)))))
    ((Tuple b v)
     (is (== v "two"))
     (is (== (ordtree2:lookup b (OrdTreeTestEntry 2 "")) None))))

  ;; delete - miss
  (match (ordtree2:update a (OrdTreeTestEntry 0 "")
                          (fn (e)
                            (match e
                              ((None) (Tuple None "nope"))
                              ((Some _) (Tuple None "huh?")))))
    ((Tuple b v)
     (is (== v "nope"))))

  ;; insert
  (match (ordtree2:update a (OrdTreeTestEntry 0 "")
                          (fn (e)
                            (match e
                              ((None)  (Tuple (Some (OrdTreeTestEntry 0 "zero"))
                                              "yeah"))
                              ((Some _) (Tuple None "huh?")))))
    ((Tuple b v)
     (is (== v "yeah"))
     (is (== (ordtree2:lookup b (OrdTreeTestEntry 0 ""))
             (Some (OrdTreeTestEntry 0 "zero"))))))

  ;; replace
  (match (ordtree2:update a (OrdTreeTestEntry 3 "")
                          (fn (e)
                            (match e
                              ((None)  (Tuple None "huh?"))
                              ((Some (OrdTreeTestEntry _ v))
                               (Tuple (Some (OrdTreeTestEntry 3 "san")) v)))))
    ((Tuple b v)
     (is (== v "three"))
     (is (== (ordtree2:lookup b (OrdTreeTestEntry 3 ""))
             (Some (OrdTreeTestEntry 3 "san"))))))
  )

(coalton-toplevel
  (define (list->ordtree lis)
    (the (ordtree2:OrdTree :a) (iter:collect! (iter:into-iter lis)))))

(define-test ordtree2-instance-test ()
  (let a = (the (ordtree2:OrdTree Integer) (list->ordtree (range 0 10))))

  (is (== (foldr Cons Nil a)
          (range 0 10)))
  (is (== (reverse (fold (flip Cons) Nil a))
          (range 0 10)))

  (is (== (the (ordtree2:OrdTree Integer) (list->ordtree (reverse (range 0 10))))
          a))
  )

(define-test ordtree2-neighbors-test ()
  (is (== (ordtree2:max-element (list->ordtree (make-list 1 2 3 4 5 6 7 8 9)))
          (Some 9)))
  (is (== (ordtree2:min-element (list->ordtree (make-list 1 2 3 4 5 6 7 8 9)))
          (Some 1)))
  (is (== (ordtree2:max-element (list->ordtree (make-list 1)))
          (Some 1)))
  (is (== (ordtree2:min-element (list->ordtree (make-list 1)))
          (Some 1)))
  (is (== (ordtree2:max-element (the (ordtree2:OrdTree Integer) ordtree2:empty))
          None))
  (is (== (ordtree2:min-element (the (ordtree2:OrdTree Integer) ordtree2:empty))
          None))

  (let ((t (list->ordtree (the (List Integer) (make-list 1 3 5 7 9)))))

    (is (== (ordtree2:lookup-neighbors t 0)
            (Tuple3 None None (Some 1))))
    (is (== (ordtree2:lookup-neighbors t 1)
            (Tuple3 None (Some 1) (Some 3))))
    (is (== (ordtree2:lookup-neighbors t 2)
            (Tuple3 (Some 1) None (Some 3))))
    (is (== (ordtree2:lookup-neighbors t 3)
            (Tuple3 (Some 1) (Some 3) (Some 5))))
    (is (== (ordtree2:lookup-neighbors t 4)
            (Tuple3 (Some 3) None (Some 5))))
    (is (== (ordtree2:lookup-neighbors t 5)
            (Tuple3 (Some 3) (Some 5) (Some 7))))
    (is (== (ordtree2:lookup-neighbors t 6)
            (Tuple3 (Some 5) None (Some 7))))
    (is (== (ordtree2:lookup-neighbors t 7)
            (Tuple3 (Some 5) (Some 7) (Some 9))))
    (is (== (ordtree2:lookup-neighbors t 8)
            (Tuple3 (Some 7) None (Some 9))))
    (is (== (ordtree2:lookup-neighbors t 9)
            (Tuple3 (Some 7) (Some 9) None)))
    (is (== (ordtree2:lookup-neighbors t 10)
            (Tuple3 (Some 9) None None))))

  ;; edge cases
  (is (== (ordtree2:lookup-neighbors ordtree2:Empty 1)
          (Tuple3 None None None)))
  (is (== (ordtree2:lookup-neighbors (list->ordtree (make-list 1)) 1)
          (Tuple3 None (Some 1) None)))
  )

;;
;; The following code is for simple benchmarking of new and old implementations
;; of OrdTree.  To run it, you need to put old ordtree.lisp in a different
;; name and load it with a package nickname 'ordtree-old'.
;; I keep it so that the code is recorded in the main commit history, just in
;; case if someone wants to redo the bench.  Can be deleted in future.
;;

#+ignore
(coalton-toplevel
  (define (ordtree-bench ndata)
    (let bigdata = (map (fn (k) (bits:and #xffffffff (* k 2654435761)))
                        (range 0 ndata)))
    (let nrepeat = (unwrap-as UFix (math:ceiling/ 1048576 ndata)))
    (let acctime = (array:make 6 0))
    (let inctime! =
      (fn (dt kind)
        (array:set! acctime kind (+ dt (array:aref acctime kind)))))

    (experimental:dotimes (_n nrepeat)

      (let (Tuple ma ta0) =
        (time (fn () (fold ordtree:insert (the (ordtree:OrdTree Integer)
                                               ordtree:empty)
                           bigdata))))
      (inctime! ta0 0)

      (let (Tuple _ ta1) =
        (time (fn () (fold (fn (_ k) (ordtree:lookup ma k)) None bigdata))))
      (inctime! ta1 1)

      (let (Tuple _ ta2) =
        (time (fn () (fold (fn (m k) (ordtree:remove m k)) ma bigdata))))
      (inctime! ta2 2)

      (let (Tuple mb tb0) =
        (time (fn () (fold ordtree-old:insert-or-replace
                           ordtree-old:empty bigdata))))
      (inctime! tb0 3)

      (let (Tuple _ tb1) =
        (time (fn () (fold (fn (_ k) (ordtree-old:lookup mb k)) None bigdata))))
      (inctime! tb1 4)

      (let (Tuple _ tb2) =
        (time (fn () (fold (fn (m k)
                             (match (ordtree-old:remove m k)
                               ((None) m)
                               ((Some mm) mm)))
                           mb bigdata))))
      (inctime! tb2 5))

    (lisp :a (ndata nrepeat acctime)
      (cl:format cl:t "~%(ndata ~6d repeat ~6d~%~
                       ;Brother tree:~%~
                       :insert ~8d~%~
                       :lookup ~8d~%~
                       :delete ~8d~%~
                       ;Red-Black tree:~%~
                       :insert ~8d~%~
                       :lookup ~8d~%~
                       :delete ~8d~%)"
                 ndata nrepeat
                 (cl:floor (cl:aref acctime 0) nrepeat)
                 (cl:floor (cl:aref acctime 1) nrepeat)
                 (cl:floor (cl:aref acctime 2) nrepeat)
                 (cl:floor (cl:aref acctime 3) nrepeat)
                 (cl:floor (cl:aref acctime 4) nrepeat)
                 (cl:floor (cl:aref acctime 5) nrepeat))))
  )

#+ignore
(define-test ordtree-simple-bench ()
  (ordtree-bench 4096)
  (ordtree-bench 16384)
  (ordtree-bench 65536)
  (ordtree-bench 262144)
  (ordtree-bench 1048576))
