(in-package #:coalton-native-tests)

(define-test ordtree2-test ()
  (let a = (the (ordtree2:Tree Integer) ordtree2:empty))

  (is (ordtree2:empty? a))
  (is (== (ordtree2:lookup a 1) None))

  (let b = (ordtree2:insert a 1))
  (is (== (ordtree2:lookup b 1) (Some 1)))

  (let data = (map (fn (k) (bits:and #xffffffff (* k 2654435761)))
                   (range 0 131072)))

  #+ignore
  (let c = (fold ordtree2:insert b data))

  (let ((t0 (time (fn ()
                    (let ((c (fold ordtree2:insert a data)))
                      (fold (fn (_ k) (ordtree2:lookup c k)) None data)))))
        (t1 (time (fn ()
                    (let ((c (fold ordtree:insert-or-replace
                                   ordtree:empty data)))
                      (fold (fn (_ k) (ordtree:lookup c k)) None data))))))
    (lisp :a (t0 t1) (cl:format cl:t "~s~%~s~%" t0 t1)))

  #+ignore
  (is (== (fold (fn (r k) (match (ordtree2:lookup c k)
                            ((None) (Cons k r))
                            ((Some _) r)))
                Nil
                data)
          Nil))
  )
