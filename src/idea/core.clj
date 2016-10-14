(ns idea.core
  (:require [incanter.core :as i :refer [$=]]
            [incanter.stats :as s]))

(defn member
  [well-being cognitive-effort]
  {:well-being well-being
   :cognitive-effort cognitive-effort})

(defn rand-member
  "Generates a random ideal audience member."
  (member (dec (rand 2)) (rand)})

(defn rand-audience
  "Generates a random audience with n members."
  [n] (repeatedly n rand-member)))

(defn measure-audience
  "apply f to every item in audience, and average and weight the result.
  f is function that returns a scalar.
  Note that almost all basic fns take this form."
  [weight f audience]
  (->> audience
       (map f)
       (s/mean)
       (* weight)))

(def disgust
  (partial measure-audience 1/2 #(- 1 (:well-being %))))

(def indifference
  (comp #(- 1 %) (partial measure-audience 1 :well-being)))

(def popularity
  (partial measure-audience 1/2 #(inc (:well-being %))))

(def provocation
  (partial measure-audience 1 :effort))

(defn divisiveness
  [audience]
  (let [well-beings (map :well-being audience)
        mean        (s/mean well-beings)
        deviation   (fn [wb] (- wb mean))]
    (->> well-beings (map deviation) (s/mean))))

(defn acquired-taste
  [a] (/ (+ (popularity a) (provocation a)) 2))

(defn instant-appeal
  [a] (/ (inc (- (popularity a) (provocation a))) 2))

(defn opinion-splitting
  [a] (/ (inc (- (divisiveness a) (provocation a))) 2))

(defn opinion-forming
  [a] (/ (+ (divisiveness a) (provocation a)) 2))

(defn shock
  [a] (/ (inc (- (disgust a) (provocation a))) 2))

(defn subversion
  [a] (/ (+ (disgust a) (provocation a)) 2))

(defn triviality
  [a] (/ (inc (- (disgust a) (provocation a))) 2))
