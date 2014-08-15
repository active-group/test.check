;   Copyright (c) Rich Hickey, Reid Draper, and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:author "Mike Sperber, adapting from Lennart Augustsson's Haskell version"
      :doc "A purely functional random-number generator."}
  clojure.test.check.random)

; This is a purely functional random generator, based on Lennart
; Augustsson's generator shipped with Hugs.

; Its comment says:

; This implementation uses the Portable Combined Generator of L'Ecuyer
; ["System.Random\#LEcuyer"] for 32-bit computers, transliterated by
; Lennart Augustsson.  It has a period of roughly 2.30584e18.

(defrecord RandomGenerator [s1 s2])

(defn make-random-generator
  "Make a random-number generator from a seed."
  [s]
  (if (neg? s)
    (make-random-generator (- s))
    (let [q (quot s 2147483562)
          s1 (rem s 2147483562)
          s2 (rem q 2147483398)]
      (RandomGenerator. (+ 1 s1) (+ 1 s2)))))

(defn random-generator-next
  "Yield a long number from the random generator and a new generator."
  [^RandomGenerator rg]
  (let [s1 (:s1 rg)
        s2 (:s2 rg)
        k (quot s1 53668)
        k* (quot s2 52774)
        s1*  (- (* 40014 (- s1 (* k 53668)))
                (* k 12211))
        s2* (- (* 40692 (- s2 (* k* 52774)))
               (* k* 3791))
        s1** (if (neg? s1*)
               (+ s1* 2147483563)
               s1*)
        s2** (if (neg? s2*)
               (+ s2* 2147483399)
               s2*)
        z (- s1** s2**)
        z* (if (< z 1)
             (+ z 2147483562)
             z)]
    [z* (RandomGenerator. s1** s2**)]))

(defn random-generator-split
  "Split a random-number generator into two."
  [rg]
  (let [s1 (:s1 rg)
        s2 (:s2 rg)
        new-s1 (if (= s1 2147483562)
                 1
                 (+ s1 1))
        new-s2 (if (= s2 1)
                 2147483398
                 (- s2 1))
        [_ nrg] (random-generator-next rg)]
    [(RandomGenerator. new-s1
                       (:s2 nrg))
     (RandomGenerator. (:s1 nrg)
                       new-s2)]))

; The intervals are inclusive.

(declare ilogbase)

(defn random-bigint
  "Yield a random bigint from low to high inclusive, and a new rng."
  [rg low high]
  (let [b 2147483561N
	k (+ (- high low) 1)]
    (loop [n (ilogbase b k)
           acc low
           rg rg]
      (if (zero? n)
	  [(+ low (mod acc k))
           rg]
          (let [[x rgn] (random-generator-next rg)]
            (recur (- n 1) (+ x (* acc b)) rgn))))))

(defn random-long
  "Yield a random long from low to high inclusive, and a new rng."
  [rg low high]
  (let [[x rng] (random-bigint rg (bigint low) (bigint high))]
    [(long x) rng]))

(def min-bound (bigint (- (bit-shift-left 1 62))))
(def max-bound (bigint (- (bit-shift-left 1 62) 1)))
(def long-range (- max-bound min-bound))

(defn random-double
  "Yield a random float from low to high inclusive, and a new rng."
  [rg low high]
  (let [low (double low)
        high (double high)
        [x nrg] (random-bigint rg min-bound max-bound)]
    (let [scaled-x (+ (/ (+ low high) 2)
                      (* (/ (- high low) long-range)
                         x))]
      [scaled-x nrg])))

(defn- ilogbase
  [b i]
  (if (< i b)
      1
      (+ 1 (ilogbase b (quot i b)))))
