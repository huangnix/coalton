(coalton-library/utils:defstdlib-package :coalton-library/ordmap
  (:use
   :coalton
   :coalton-library/classes
   :coalton-library/hash
   :coalton-library/tuple
   :coalton-library/functions)
  (:local-nicknames
   (#:tree :coalton-library/ordtree)
   (#:iter :coalton-library/iterator))
  (:shadow #:empty)
  (:export
   #:OrdMap
   #:empty
   #:lookup
   #:lookup-neighbors
   #:max-key-entry
   #:min-key-entry
   #:insert
   #:adjoin
   #:replace
   #:remove
   #:update
   #:keys
   #:values
   #:entries
   #:collect!
   #:collect
   #:merge))

(in-package :coalton-library/ordmap)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (define-type (MapPair :key :value)
    (MapPair :key :value)

    ;; hack: when searching for an entry, we need a thing that holds just a key, but is of type `MapPair :key
    ;; :any', to pass to `tree:lookup' or `tree:remove'. A `OrdMap' will never hold a `JustKey'.
    (JustKey :key))

  (define (key pair)
    (match pair
      ((MapPair k _) k)
      ((JustKey k) k)))

  (define (value pair)
    (match pair
      ((MapPair _ v) v)
      (_ (error "Misuse of `JustKey': Cannot read the Value of a JustKey"))))

  (define-instance (Into (MapPair :key :value) (Tuple :key :value))
    (define (into pair)
      (Tuple (key pair) (value pair))))

  (define-instance ((Eq :key) => Eq (MapPair :key :value))
    (define (== left right)
      (== (key left) (key right))))

  (define-instance ((Ord :key) => Ord (MapPair :key :value))
    (define (<=> left right)
      (<=> (key left) (key right))))

  (repr :transparent)
  (define-type (OrdMap :key :value)
    "A red-black binary tree which associates each :KEY with a :VALUE, sorted by `<=>' on the keys and unique by `==' on the keys."
    (%Map (tree:Tree (MapPair :key :value))))

  ;; Mapping API
  (declare empty (OrdMap :key :value))
  (define empty
    "A OrdMap containing no mappings."
    (%Map tree:Empty))

  ;; Mapping API
  (declare lookup ((Ord :key) => OrdMap :key :value -> :key -> Optional :value))
  (define (lookup mp k)
    "Retrieve the value associated with K in MP, or None if MP does not contain K."
    (let (%Map tre) = mp)
    (map value (tree:lookup tre (JustKey k))))

  (declare max-key-entry ((Ord :key) => OrdMap :key :value -> (Optional (Tuple :key :value))))
  (define (max-key-entry mp)
    "Returns the entry (Tuple :key :value) with the maximum key in the map `mp`. If the map is empty, None is returned."
    (let (%Map tre) = mp)
    (map into (tree:max-element tre)))

  (declare min-key-entry ((Ord :key) => OrdMap :key :value -> (Optional (Tuple :key :value))))
  (define (min-key-entry mp)
    "Returns the entry (Tuple :key :value) with the minimum key in the map `mp`. If the map is empty, None is returned."
    (let (%Map tre) = mp)
    (map into (tree:min-element tre)))

  (declare lookup-neighbors ((Ord :key) => OrdMap :key :value -> :key
                             -> (Tuple3 (Optional (Tuple :key :value))
                                        (Optional (Tuple :key :value))
                                        (Optional (Tuple :key :value)))))
  (define (lookup-neighbors mp k)
    "Returns elements LO, ON, and HI, such that LO has the closest
key that is strictly less than `k`, ON is the entry with `k`,
and HI has the closest key that is strictly greater than 'k'.
Any of these values can be None if there's no such entry."
    (let (%Map tre) = mp)
    (let ((declare convert ((Optional (MapPair :key :value))
                            -> Optional (Tuple :key :value)))
          (convert (fn (p)
                     (do (x <- p)
                         (Some (into x))))))
      (match (tree:lookup-neighbors tre (JustKey k))
        ((Tuple3 lo on hi)
         (Tuple3 (convert lo) (convert on) (convert hi))))))

  ;; Mapping API
  (declare insert (Ord :key => OrdMap :key :value -> :key -> :value -> OrdMap :key :value))
  (define (insert mp k v)
    "Returns an OrdMap in which the key `k` is associated with `v` added
to the `mp`.  If `mp` already contains mapping for `k`, it is replaced."
    (let (%Map tre) = mp)
    (%Map (tree:insert-or-replace tre (MapPair k v))))

  ;; Mapping API
  (declare adjoin (Ord :key => OrdMap :key :value -> :key -> :value -> OrdMap :key :value))
  (define (adjoin mp k v)
    "Returns an OrdMap in which the key `k` is associated with `v` added
to the `mp`, only when `mp` doesn't have an association with `k`.
If `mp` already contains an association with `k`, `mp` is returned as is."
    (let (%Map tre) = mp)
    (match (tree:insert tre (MapPair k v))
      ((None) mp)
      ((Some mp1) (%Map mp1))))

  ;; Mapping API
  (declare replace (Ord :key => OrdMap :key :value -> :key -> :value -> OrdMap :key :value))
  (define (replace mp k v)
    "Returns an OrdMap in which the key `k` is associated with `v` replaced
from `mp`, when `mp` already has an association with `k`.
If `mp` doesn't has an association with `k`, `mp` is returned as is."
    (let (%Map tre) = mp)
    (match (tree:replace tre (MapPair k v))
      ((None) mp)
      ((Some (Tuple tre _)) (%Map tre))))

  ;; Mapping API
  (declare remove (Ord :key => OrdMap :key :value -> :key -> OrdMap :key :value))
  (define (remove mp k)
    "Returns an OrdMap in which the association with key 'k' is removed from
`mp`.  If `mp` doesn't have an association with `k`, it is returned as is."
    (let (%Map tre) = mp)
    (match (tree:remove tre (JustKey k))
      ((None) mp)
      ((Some mp1) (%Map mp1))))

  #+obsoleted
  (declare insert ((Ord :key) => ((OrdMap :key :value) -> :key -> :value -> (Optional (OrdMap :key :value)))))
  #+obsoleted
  (define (insert mp k v)
    "Associate K with V in MP. If MP already contains a mapping for K, return None."
    (let (%Map tre) = mp)
    (map %Map (tree:insert tre (MapPair k v))))

  #+obsoleted
  (declare replace ((Ord :key) => ((OrdMap :key :value) -> :key -> :value -> (Optional (Tuple (OrdMap :key :value)
                                                                                           :value)))))
  #+obsoleted
  (define (replace mp k v)
    "Change the association of K to V in MP. If MP did not already contain a mapping for K, return None."
    (let (%Map tre) = mp)
    (match (tree:replace tre (MapPair k v))
      ((None) None)
      ((Some (Tuple new-tre removed-pair))
       (Some (Tuple (%Map new-tre)
                    (value removed-pair))))))

  #+obsoleted
  (declare replace-or-insert ((Ord :key) => ((OrdMap :key :value) -> :key -> :value -> (Tuple (OrdMap :key :value)
                                                                                           (Optional :value)))))
  #+obsoleted
  (define (replace-or-insert mp k v)
    "Update MP to associate K with V.

If MP already contains a mapping for K, replace it and return the old value."
    (let (%Map tre) = mp)
    (let (Tuple new-tre removed-pair) = (tree:replace-or-insert tre (MapPair k v)))
    (Tuple (%Map new-tre) (map value removed-pair)))

  #+obsoleted
  (declare insert-or-replace ((Ord :key) => ((OrdMap :key :value) -> :key -> :value -> (OrdMap :key :value))))
  #+obsoleted
  (define (insert-or-replace mp k v)
    "Update MP to associate K with V.

If MP already contains a mapping for K, replace it and discard the old value.

Like `replace-or-insert', but prioritizing insertion as a use case."
    (let (%Map tre) = mp)
    (%Map (tree:insert-or-replace tre (MapPair k v))))

  #+obsoleted
  (declare remove ((Ord :key) => ((OrdMap :key :value) -> :key -> (Optional (OrdMap :key :value)))))
  #+obsoleted
  (define (remove mp k)
    "Remove the mapping associated with K in MP. If K does not have a value in MP, return None."
    (let (%Map tre) = mp)
    (map %Map (tree:remove tre (JustKey k))))

  (declare entries ((OrdMap :key :value) -> (iter:Iterator (Tuple :key :value))))
  (define (entries mp)
    "Iterate over the (key value) pairs in MP, sorted by the keys in least-to-greatest order."
    (match mp
      ((%Map tre) (map into (tree:increasing-order tre)))))

  (define-instance (iter:IntoIterator (OrdMap :key :value) (Tuple :key :value))
    (define iter:into-iter entries))

  (declare keys ((OrdMap :key :value) -> (iter:Iterator :key)))
  (define (keys mp)
    "Iterate over the keys in MP, sorted least-to-greatest."
    (match mp
      ((%Map tre) (map key (tree:increasing-order tre)))))

  (declare values ((OrdMap :key :value) -> (iter:Iterator :value)))
  (define (values mp)
    "Iterate over the values in MP, sorted by their corresponding keys in least-to-greatest order."
    (match mp
      ((%Map tre) (map value (tree:increasing-order tre)))))

  (define-instance ((Eq :key) (Eq :value) => Eq (OrdMap :key :value))
    (define (== left right)
      (iter:elementwise-match! == (entries left) (entries right))))

  (define-instance ((Hash :key) (Hash :value) => Hash (OrdMap :key :value))
    (define (hash mp)
      (iter:elementwise-hash! (entries mp))))

  (declare collect! ((Ord :key) => ((iter:Iterator (Tuple :key :value)) -> (OrdMap :key :value))))
  (define (collect! iter)
    "Construct a `OrdMap` containing all the `(key value)` pairs in `iter`.

If `iter` contains duplicate keys, later values will overwrite earlier values."
    (iter:fold! (fn (mp tpl)
                  (uncurry (insert mp) tpl))
                empty
                iter))

  (declare collect ((Ord :key) (Foldable :collection) => ((:collection (Tuple :key :value)) -> (OrdMap :key :value))))
  (define (collect coll)
    "Construct a `OrdMap` containing all the `(key value)` pairs in `coll`.

If `coll` contains duplicate keys, later values will overwrite earlier values."
    (fold (fn (mp tpl)
            (uncurry (insert mp) tpl))
          empty
          coll))

  (define-instance (Ord :key => iter:FromIterator (OrdMap :key :value) (Tuple :key :value))
    (define iter:collect! collect!))


  ;; Mapping API
  (declare update (Ord :key => OrdMap :key :value -> :key
                       -> (Optional :value -> Tuple (Optional :value) :a)
                       -> Tuple (OrdMap :key :value) :a))
  (define (update mp k f)
    "Lookup an association with `k` in `mp`.  If there's an entry, call `f`
with its value wrapped with Some.  If there isn't an entry, call 'f' with
None.  `f` must terun a tuple of possible new value and an auxiliary
result.
If the fst of `f`'s return value is Some, its content is inserted into
`mp` in association with `k`.   If the fst of `f`'s return value is None,
an association with `k` in `mp` is removed.  A possibly updated mapping
is returned as the fst element of the tuple.
The auxiliary result from `f` is returnd as the snd result.

This can be used for the caller to obtain the previous state along
updated map.  For example, the following code inserts an entry (k, v)
into mp, and obtain (Some v') or None in the second value of the
result, where v' is the previous value associated with k.


```
(update mp k (Tuple v))
```
"
    (let (%Map tre) = mp)
    ;; Temporary solution; should be implemented in tree level.
    (match (tree:lookup tre (JustKey k))
      ((None)
       (match (f None)
         ((Tuple (None) aux) (Tuple mp aux))
         ((Tuple (Some new-v) aux)
          (Tuple (%Map (tree:insert-or-replace tre (MapPair k new-v))) aux))))
      ((Some (MapPair _ old-v))
       (match (f (Some old-v))
         ((Tuple (None) aux)
          (match (tree:remove tre (JustKey k))
            ((None) (error "this can't happen"))
            ((Some tre) (Tuple (%Map tre) aux))))
         ((Tuple (Some new-v) aux)
          (Tuple (%Map (tree:insert-or-replace tre (MapPair k new-v))) aux))))
      ((Some (JustKey _))
       (error "OrdMap contains `JustKey` rather than `MapPair`"))))

  #+obsoleted
  (declare update ((Ord :key) => (:value -> :value) -> (OrdMap :key :value) -> :key -> (Optional (OrdMap :key :value))))
  #+obsoleted
  (define (update func mp key)
    "Apply FUNC to the value corresponding to KEY in MP, returning a new `OrdMap' which maps KEY to the result of the function."
    (let (%Map tre) = mp)
    (let ((helper
            (fn (subtree)
              (match subtree
                ((tree:Empty) None)
                ;; `Branch' intentionally unexported from the tree package, but needed here because this
                ;; operation does tree search but is meaningless on trees that aren't maps.
                ((tree::Branch clr less (MapPair pivot val) more)
                 (match (<=> key pivot)
                   ((LT) (map (fn (new-less)
                                (tree::Branch clr new-less (MapPair pivot val) more))
                              (helper less)))
                   ((Eq) (Some (tree::Branch clr less (MapPair pivot (func val)) more)))
                   ((GT) (map (tree::Branch clr less (MapPair pivot val))
                              (helper more)))))
                ((tree::Branch _ _ (JustKey _) _) (error "OrdMap contains `JustKey` rather than `MapPair`"))
                ((tree::DoubleBlackEmpty) (error "Encountered double-black node in ordered map outside of removal operation"))))))
      (map %Map (helper tre))))

  (declare merge (Ord :key => OrdMap :key :value -> OrdMap :key :value -> OrdMap :key :value))
  (define (merge a b)
    "Construct a Tree containing all the mappings of both A and B.

If A and B contain mappings X -> A' and X -> B', it is undefined whether the result maps X to A' or B'.

Because of the possibility that A and B will map the same X to different A' and B', this is not an associative
operation, and therefore OrdMap cannot implement Monoid."
    (let (%Map a) = a)
    (let (%Map b) = b)
    (%Map (tree:merge a b)))

  (define-instance (Ord :key => Semigroup (OrdMap :key :value))
    (define <> merge))

  (define-instance (Functor (OrdMap :key))
    (define (map func mp)
      (let (%Map tre) = mp)
      (let ((helper (fn (subtree)
                      (match subtree
                        ((tree::Empty) tree:Empty)
                        ((tree::Branch clr left (MapPair key value) right)
                         (tree::Branch clr
                                       (helper left)
                                       (MapPair key (func value))
                                       (helper right)))
                        ((tree::Branch _ _ (JustKey _) _) (error "OrdMap contains `JustKey` rather than `MapPair`"))
                        ((tree::DoubleBlackEmpty) (error "Encountered double-black node in ordered map outside of removal operation"))))))
        (%Map (helper tre)))))

  ;; As with `tree:Tree', `OrdMap' should probably implement `Traversable'.
  )
