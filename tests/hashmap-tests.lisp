(in-package #:coalton-native-tests)

(define-test hashmap-test ()
  (let a = (the (hashmap:HashMap String Integer) hashmap:empty))
  (is (hashmap:empty? a))
  (is (== (hashmap:count a) 0))

  (let b = (hashmap:insert a "alpha" 1))
  (is (not (hashmap:empty? b)))
  (is (== (hashmap:count b) 1))

  (let c = (hashmap:insert b "beta" 2))
  (is (not (hashmap:empty? c)))
  (is (== (hashmap:count c) 2))

  (let d = (hashmap:insert c "gamma" 3))
  (let e = (hashmap:insert d "delta" 4))

  (is (== (Some 1) (hashmap:lookup e "alpha")))
  (is (== (Some 2) (hashmap:lookup e "beta")))
  (is (== (Some 3) (hashmap:lookup e "gamma")))
  (is (== (Some 4) (hashmap:lookup e "delta")))
  (is (== None (hashmap:lookup e "epsilon")))

  (let f = (hashmap:insert e "alpha" 5))
  (is (== (Some 5) (hashmap:lookup f "alpha")))

  (let g = (hashmap:remove f "beta"))
  (is (== (Some 5) (hashmap:lookup g "alpha")))
  (is (== None (hashmap:lookup g "beta")))
  (is (== (Some 3) (hashmap:lookup g "gamma")))
  (is (== (Some 4) (hashmap:lookup g "delta")))

  (let h = (hashmap:remove g "epsilon"))
  (is (== (Some 5) (hashmap:lookup g "alpha")))
  (is (== (Some 3) (hashmap:lookup g "gamma")))
  (is (== (Some 4) (hashmap:lookup g "delta")))

  (let i = (hashmap:insert h "alpha" 5))
  (let j = (hashmap:insert i "gamma" 3))
  (let k = (hashmap:insert j "delta" 4))
  (is (== (Some 5) (hashmap:lookup g "alpha")))
  (is (== (Some 3) (hashmap:lookup g "gamma")))
  (is (== (Some 4) (hashmap:lookup g "delta")))

  (let z = (the (List (Tuple String Integer))
                (iter:collect! (iter:into-iter g))))
  (is (== (into (list:length z)) (hashmap:count g)))
  (is (iter:every! (fn ((Tuple k v))
                     (match (hashmap:lookup g k)
                       ((None) False)
                       ((Some vv) (== v vv))))
                   (iter:into-iter z)))

  (let i = (hashmap:remove (hashmap:remove (hashmap:remove h "alpha") "gamma") "delta"))
  (is (hashmap:empty? i))

  ;; Update protocol
  ;;  In the following tests, the insertion of keys are chosen so that
  ;;  internal tree takes specific shapes.  If we ever change hash function,
  ;;  the keys must be adjusted accordingly.


  ;; utility.  delete k, returns prev value
  (let t-delete = (fn (m k)
                    (hashmap:update m k
                                    (fn (v)
                                      (match v
                                        ((None) (Tuple None "huh?"))
                                        ((Some x) (Tuple None x)))))))
  ;; utility.  replace k, returns prev value
  (let t-replace = (fn (m k v)
                     (hashmap:update m k
                                     (fn (v0)
                                       (match v0
                                         ((None) (Tuple None "huh?"))
                                         ((Some x) (Tuple (Some v) x)))))))
  ;; utility.  insert k, returns "yeah" on success
  (let t-insert = (fn (m k v)
                    (hashmap:update m k
                                    (fn (v0)
                                      (match v0
                                        ((None) (Tuple (Some v) "yeah"))
                                        ((Some _) (Tuple v0 "huh?")))))))


  ;; One 'Leaf' node
  (let m_leaf = (hashmap:insert hashmap:empty 1 "ichi"))

  (match (t-replace m_leaf 1 "hitotsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 1) (Some "hitotsu")))
     (is (== aux "ichi"))))

  (match (t-insert m_leaf 2 "ni")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 2) (Some "ni")))
     (is (== aux "yeah"))))

  (match (t-delete m_leaf 1)
    ((Tuple m aux)
     (is (== (hashmap:lookup m 1) None))
     (is (hashmap:empty? m))
     (is (== aux "ichi"))))

  ;; One 'Bud' node
  (let m_bud = (hashmap:insert m_leaf 2 "ni"))

  (match (t-replace m_bud 2 "futatsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 2) (Some "futatsu")))
     (is (== (hashmap:lookup m 1) (Some "ichi")))
     (is (== aux "ni"))))

  (match (t-insert m_bud 3 "mittsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 1) (Some "ichi")))
     (is (== (hashmap:lookup m 2) (Some "ni")))
     (is (== (hashmap:lookup m 3) (Some "mittsu")))
     (is (== aux "yeah"))))

  (match (t-delete m_bud 2)
    ((Tuple m aux)
     (is (== (hashmap:lookup m 2) None))
     (is (== (hashmap:lookup m 1) (Some "ichi")))
     (is (== aux "ni"))
     (match (t-delete m 2)
       ((Tuple _ aux)
        (is (== aux "huh?"))))
     (match (t-delete m 1)
       ((Tuple m aux)
        (is (hashmap:empty? m))
        (is (== aux "ichi"))))))

  ;; One 'Tree' node, with 3 leaves
  (let m_tree_leaf = (hashmap:insert m_bud 3 "san"))

  (match (t-replace m_tree_leaf 3 "mittsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 3) (Some "mittsu")))
     (is (== (hashmap:lookup m 2) (Some "ni")))
     (is (== (hashmap:lookup m 1) (Some "ichi")))
     (is (== aux "san"))))

  (match (t-insert m_tree_leaf 4 "yottsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 4) (Some "yottsu")))
     (is (== (hashmap:lookup m 3) (Some "san")))
     (is (== (hashmap:lookup m 2) (Some "ni")))
     (is (== (hashmap:lookup m 1) (Some "ichi")))
     (is (== aux "yeah"))))

  (match (t-delete m_tree_leaf 1)
    ((Tuple m aux)
     (is (== (hashmap:lookup m 3) (Some "san")))
     (is (== (hashmap:lookup m 2) (Some "ni")))
     (is (== (hashmap:lookup m 1) None))
     (is (== aux "ichi"))
     ;; ensure the tree shrinks as supposed
     (match (t-delete m 2)
       ((Tuple m aux)
        (is (== aux "ni"))
        (match (t-delete m 3)
          ((Tuple m aux)
           (is (== aux "san"))
           (is (hashmap:empty? m))))))))

  ;; this creates a bud with 1 and 32 under the toplevel tree
  (let m_tree_bud = (hashmap:insert m_tree_leaf 32 "sanjuuni"))

  (match (t-replace m_tree_bud 1 "hitotsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 1) (Some "hitotsu")))
     (is (== (hashmap:lookup m 32) (Some "sanjuuni")))
     (is (== aux "ichi"))))

  (match (t-insert m_tree_bud 4 "yottsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 4) (Some "yottsu")))
     (is (== (hashmap:lookup m 32) (Some "sanjuuni")))
     (is (== (hashmap:lookup m 3) (Some "san")))
     (is (== (hashmap:lookup m 2) (Some "ni")))
     (is (== (hashmap:lookup m 1) (Some "ichi")))
     (is (== aux "yeah"))))

  (match (t-delete m_tree_bud 1)
    ((Tuple m aux)
     (is (== (hashmap:lookup m 32) (Some "sanjuuni")))
     (is (== (hashmap:lookup m 3) (Some "san")))
     (is (== (hashmap:lookup m 2) (Some "ni")))
     (is (== (hashmap:lookup m 1) None))
     (is (== aux "ichi"))
     (match (t-delete m 32)
       ((Tuple m aux)
        (is (== aux "sanjuuni"))
        (match (t-delete m 2)
          ((Tuple m aux)
           (is (== aux "ni"))
           (match (t-delete m 3)
             ((Tuple m aux)
              (is (== aux "san"))
              (match (t-delete m 1)
                ((Tuple m aux)
                 (is (== aux "huh?"))
                 (is (hashmap:empty? m))))))))))))

  ;; this creats a tree for 1, 32, 65 under the toplevel tree
  (let m_tree_tree = (hashmap:insert m_tree_bud 65 "rokujuugo"))

  (match (t-replace m_tree_tree 1 "hitotsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 1) (Some "hitotsu")))
     (is (== (hashmap:lookup m 32) (Some "sanjuuni")))
     (is (== (hashmap:lookup m 65) (Some "rokujuugo")))
     (is (== aux "ichi"))))

  (match (t-insert m_tree_tree 4 "yottsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 1) (Some "ichi")))
     (is (== (hashmap:lookup m 2) (Some "ni")))
     (is (== (hashmap:lookup m 3) (Some "san")))
     (is (== (hashmap:lookup m 4) (Some "yottsu")))
     (is (== (hashmap:lookup m 32) (Some "sanjuuni")))
     (is (== (hashmap:lookup m 65) (Some "rokujuugo")))
     (is (== aux "yeah"))))

  (match (t-delete m_tree_tree 2)
    ((Tuple m aux)
     (is (== (hashmap:lookup m 2) None))
     (is (== aux "ni"))
     (match (t-delete m 3)
       ((Tuple m aux)
        (is (== (hashmap:lookup m 3) None))
        (is (== aux "san"))
        (match (t-delete m 32)
          ((Tuple m aux)
           (is (== (hashmap:lookup m 32) None))
           (is (== aux "sanjuuni"))
           (match (t-delete m 65)
             ((Tuple m aux)
              (is (== (hashmap:lookup m 65) None))
              (is (== aux "rokujuugo"))
              (match (t-delete m 1)
                ((Tuple m aux)
                 (is (== (hashmap:lookup m 1) None))
                 (is (== aux "ichi"))
                 (is (hashmap:empty? m))))))))))))

  ;; this creats a tree for a bud with 1, 1056 under the 2nd level subtree
  (let m_tree_tree_bud = (hashmap:insert m_tree_tree 1056 "sengojuuroku"))

  (match (t-replace m_tree_tree_bud 1056 "hitomarugoroku")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 1) (Some "ichi")))
     (is (== (hashmap:lookup m 32) (Some "sanjuuni")))
     (is (== (hashmap:lookup m 65) (Some "rokujuugo")))
     (is (== (hashmap:lookup m 1056) (Some "hitomarugoroku")))
     (is (== aux "sengojuuroku"))))

  (match (t-insert m_tree_tree_bud 4 "yottsu")
    ((Tuple m aux)
     (is (== (hashmap:lookup m 4) (Some "yottsu")))
     (is (== aux "yeah"))))

  (match (t-delete m_tree_tree_bud 1)
    ((Tuple m aux)
     (is (== (hashmap:lookup m 1) None))
     (is (== (hashmap:lookup m 1056) (Some "sengojuuroku")))
     (is (== aux "ichi"))
     (match (t-insert m 1 "hitotsu")
       ((Tuple m aux)
        (is (== (hashmap:lookup m 1) (Some "hitotsu")))
        (is (== (hashmap:lookup m 1056) (Some "sengojuuroku")))
        (is (== aux "yeah"))
        (match (t-delete m 1056)
          ((Tuple m aux)
           (is (== (hashmap:lookup m 1) (Some "hitotsu")))
           (is (== (hashmap:lookup m 1056) None))
           (is (== aux "sengojuuroku"))))))
     (match (t-delete m 2)
       ((Tuple m aux)
        (is (== aux "ni"))
        (match (t-delete m 3)
          ((Tuple m aux)
           (is (== aux "san"))
           (match (t-delete m 1)
             ((Tuple m aux)
              (is (== aux "huh?"))))
           (match (t-delete m 32)
             ((Tuple m aux)
              (is (== aux "sanjuuni"))
              (match (t-delete m 65)
                ((Tuple m aux)
                 (is (== aux "rokujuugo"))
                 (match (t-delete m 1056)
                   ((Tuple m aux)
                    (is (== aux "sengojuuroku"))
                    (is (hashmap:empty? m))))))))))))))
  )

;; NB: Eventually we want a library of random data generators, similar to
;; Haskell's Test.QuickCheck.Gen or Scheme's SRFI-252 Property Testing.
;; This is a temporary stuff.
(coalton-toplevel
  (define hashmap-test/rand64
    (let ((s (cell:new 42))
          (a 2862933555777941757)
          (b 3037000493)
          (m (coalton-library/bits:shift 64 1)))
      ;; Simple 64bit congruential generator.
      ;; https://nuclear.llnl.gov/CNP/rng/rngman/node4.html
      ;; CL's random is cumbersome to use with portable fixed seed value.
      (fn ()
        (let ((ss (mod (+ (* a (cell:read s)) b) m)))
          (cell:write! s ss)
          ss))))
  )

(define-test hashmap-test-heavy ()
  (let data-size = 500000)
  (let tab = (hashtable:new))
  (let ht = (rec gen ((ht hashmap:empty)
                      (i data-size))
              (if (== i 0)
                  ht
                  (let ((k (hashmap-test/rand64))
                        (v (hashmap-test/rand64)))
                    (hashtable:set! tab k v)
                    (gen (hashmap:insert ht k v) (1- i))))))
  (is (== (hashmap:count ht) (hashtable:count tab)))

  (is (iter:every! (fn ((Tuple key val))
                     (match (hashtable:get tab key)
                       ((None) False)
                       ((Some value) (== value val))))
                   (iter:into-iter ht)))

  (let ht2 = (iter:fold! (fn (ht key) (hashmap:remove ht key))
                         ht
                         (hashtable:keys tab)))

  (is (hashmap:empty? ht2))
  )
