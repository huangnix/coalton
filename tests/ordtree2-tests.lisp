(in-package #:coalton-native-tests)

(define-test ordtree2-test ()
  (let a = (the (ordtree2:Tree Integer) ordtree2:empty))

  (is (ordtree2:empty? a))
  (is (== (ordtree2:lookup a 1) None))

  (let b = (ordtree2:insert a 1))
  (is (== (ordtree2:lookup b 1) (Some 1)))

  (let data = (map (fn (k) (bits:and #xffffffff (* k 2654435761)))
                   (range 0 16384)))

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

  )

(define-test ordtree2-simple-bench ()

  (let bigdata = (map (fn (k) (bits:and #xffffffff (* k 2654435761)))
                      (range 0 131072)))

  (let (Tuple ma ta0) =
    (time (fn () (fold ordtree2:insert (the (ordtree2:Tree Integer)
                                            ordtree2:empty)
                bigdata))))
  (let (Tuple _ ta1) =
    (time (fn () (fold (fn (_ k) (ordtree2:lookup ma k)) None bigdata))))

  (let (Tuple mb tb0) =
    (time (fn () (fold ordtree:insert-or-replace
                       ordtree:empty bigdata))))
  (let (Tuple _ tb1) =
    (time (fn () (fold (fn (_ k) (ordtree:lookup mb k)) None bigdata))))

  (lisp :a (ta0 ta1 tb0 tb1)
    (cl:format cl:t "~%Brother tree:~%~
                     insert ~s~%~
                     lookup ~s~%~
                     Red-Black tree:~%~
                     insert ~s~%~
                     lookup ~s~%"
               ta0 ta1 tb0 tb1))

  )
