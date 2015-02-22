(ns transducers.core)

(defn mmap
  "map transducer takes in a function f and applies it to all the elements of the collection"
  [f]
  (fn [rfn]
    (fn
      ([] (rfn))
      ([result] (rfn result))
      ([result input] (rfn result (f input))))))

(defn mfilter
  "filter transducer that takes in a predicate p and filters elements of the collection"
  [p]
  (fn [rfn]
    (fn
      ([] (rfn))
      ([result] (rfn result))
      ([result input] (if (p input)
                        (rfn result input)
                        result)))))

(defn mremove
  "remove transducer that takes in a predicate p and removes elements of the collection"
  [p]
  (mfilter (complement p)))

(defn logger-rfn
  "A logger transducer that takes no argument and log what happens under the scene based on the arity"
  []
  (fn [rfn]
    (fn
      ([]
       (println "Artity 0")
       (rfn))
      ([result]
       (do
         (println "Arity 1: " result)
         (rfn result)))
      ([result input]
       (do
         (println "Arity 2: " result " " input)
         (rfn result input))))))

(defn mdedupe
  "A transducer that takes in no argument and performs a dedupe operation"

  []
  (fn [rfn]
    (let [prev (volatile! ::none)]
      (fn
        ([] (rfn))
        ([result] (rfn result))
        ([result input]
         (let [prior @prev]
           (vreset! prev input)
           (if (= input prior)
             result
             (rfn result input))))))))

(defn mdedupe-with
  "A transducer that takes in a function to dedupe with
  Eg: (into [] (mdedupe-with even?) [1 2 2 4 4 5])
  => [1 2 5]"
  [f]
  (fn [rfn]
    (let [prev (volatile! ::none)]
      (fn
        ([] (rfn))
        ([result] (rfn result))
        ([result input]
         (let [prior @prev]
           (vreset! prev (f input))
           (if (= (f input) prior)
             result
             (rfn result input))))))))

(defn mtake
  "The take transducer. Takes in an integer n"
  [n]
  {:pre [(>= n 0) (integer? n)]}
  (fn [rfn]
    (let [nv (volatile! n)]
      (fn
        ([] (rfn))
        ([result] (rfn result))
        ([result input]
         (let [n @nv
               nn (vswap! nv dec)
               result (if (pos? n)
                        (rfn result input)
                        result)]
           (if (not (pos? nn))
             (reduced result)
             result)))))))

(defn mdrop
  [n]
  {:pre [(>= n 0) (integer? n)]}
  (fn [rfn]
    (let [nv (volatile! n)]
      (fn
        ([] (rfn))
        ([result] (rfn result))
        ([result input]
         (let [n @nv]
           (vswap! nv dec)
           (if (pos? n)
             result
             (rfn result input))))))))

(defn mtake-while
  [p]
  (fn [rfn]
    (fn
      ([] (rfn))
      ([result] (rfn result))
      ([result input]
       (if (p input)
         (rfn result input)
         (reduced result))))))

(defn mdrop-while
  [p]
  (fn [rfn]
    (let [pv (volatile! true)]
      (fn
        ([] (rfn))
        ([result] (rfn result))
        ([result input]
         (let [drop? @pv
               pred-value (p input)]
           (if (and drop? (p input))
             result
             (do
               (vreset! pv false)
               (rfn result input)))))))))

;; Other way to define mmapcat
;; (defn mmapcat
;;   [f]
;;   (comp (mmap f) cat))

(defn mmapcat
  [f]
  (fn [rfn]
    (fn
      ([] (rfn))
      ([result] (rfn result))
      ([result input]
       (reduce rfn result (f input))))))

(defn muniq
  []
  (fn [rfn]
    (let [cv (volatile! #{})]
      (fn
        ([] (rfn))
        ([result] (rfn result))
        ([result input]
         (let [c @cv]
           (if (get c input)
             result
             (do
               (vswap! cv conj input)
               (rfn result input)))))))))

(defn mrandom-sample
  [prob]
  {:pre [(<= prob 1) (>= prob 0)]}
  (mfilter (fn [_] (< (rand) prob))))

;; (defn mpartition-by
;;   [f]
;;   (fn [rfn]
;;     (let [pv (volatile! (vector))]
;;       (fn
;;       ([] (rfn))
;;       ([result]
;;        (let [p @pv]
;;          (if-not (seq p)
;;            result
;;            (rfn result p)))
;;        (rfn result))
;;       ([result input]
;;        (let [p @pv]
;;          (if-not (seq p)
;;            (do
;;              (vswap! pv conj input)
;;              result)
;;            (if (= (f input) (f (first p)))
;;              (do
;;                (vswap! pv conj input)
;;                result)
;;              (do
;;                (vreset! pv (vector input))
;;                (rfn result p))))))))))
