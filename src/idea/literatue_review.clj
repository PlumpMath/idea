(ns idea.literatue-review
  (:require [clj-wordnet.core :as wn]
            [cc-services.metaphor-magnet :as mm]
            [cc-services.theasaurus-rex :as therex]))

(def wordnet (wn/make-dictionary "C:\\Program Files (x86)\\WordNet\\2.1\\dict\\"))

(def imitate (first (wordnet "imitate" :verb)))

;;;; how can I do more of this? i.e. other than :derivationally-related?
(map :lemma (wn/related-words imitate :derivationally-related))

(defn hypernyms
  [word]
  (println (:lemma word))
  (flatten (vals (wn/related-synsets word :hypernym))))

(therex/shared-categories "imitation" "dance")

(def antipodean-concepts
  ["imagination" "ingenuity" "innovation" "inspiration" "inventiveness" "muse"
  "novelty" "originality" "serendipity" "talent" "unique"])

(def c (map #(therex/shared-categories "imitation" %) antipodean-concepts))


(def m-hypernyms (memoize hypernyms))

(map (juxt :lemma :gloss) (apply concat (take 2 (iterate #(map hypernyms %) (hypernyms imitate)))))

(defn pword
  [w] (clojure.pprint/pprint ((juxt :lemma :pos :gloss :synonyms) w)))

(pword imitate)

(hypernyms (hypernyms imitate))
