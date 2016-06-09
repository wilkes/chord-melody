(ns chord-melody.core
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.spec.test :as test]))

(def notes [:A :Bb :B :C :Db :D :Eb :E :F :Gb :G :Ab])

(defn no-duplicates? [xs]
  (= (count xs) (count (set xs))))

(s/def ::note (set notes))

(s/def ::chord
  (s/and (s/tuple ::note ::note ::note ::note)
         no-duplicates?))

(s/def ::closed ::chord)
(s/def ::drop-2 ::chord)
(s/def ::drop-3 ::chord)

(s/def ::chord-melody-args
  (s/and (s/cat :chord ::chord :melody ::note)
         #((-> % :chord set) (-> % :melody))))

(defn inversions [chord]
  (reduce (fn [invs _]
            (let [[first & rest] (last invs)]
              (conj invs (into [] (concat rest [first])))))
          [chord]
          (rest chord)))

(s/fdef inversions
        :args (s/cat :chord ::chord)
        :ret (s/tuple ::chord ::chord ::chord ::chord)
        :fn #(and (= (-> % :args :chord) (-> % :ret first))
                  (= (-> % :args :chord count)
                     (-> % :ret count)
                     (-> % :ret set count))))

(defn drop-2 [[bass tenor alto suprano]]
  [alto bass tenor suprano])

(s/fdef drop-2
        :args (s/cat :chord ::chord)
        :ret ::chord
        :fn #(= (-> % :args :chord (nth 2))
                (-> % :ret first)))

(defn drop-3 [[bass tenor alto suprano]]
  [tenor bass alto suprano])

(s/fdef drop-3
        :args (s/cat :chord ::chord)
        :ret ::chord
        :fn #(= (-> % :args :chord (nth 1))
                (-> % :ret first)))

(defn inversion [chord melody]
  (->> chord
       inversions
       (filter #(= (last %) melody))
       first))

(s/fdef inversion
        :args ::chord-melody-args
        :ret ::chord
        :fn #(and (= (-> % :ret last)
                     (-> % :args :melody))
                  (= (-> % :args :chord set)
                     (-> % :ret set))))

(defn chord-melody-voicings [chord melody]
  (when-not (s/valid? ::chord-melody-args [chord melody])
    (throw (RuntimeException. (with-out-str (s/explain ::chord-melody-args [chord melody])))))
  (let [inv (inversion chord melody)]
    {::closed inv
     ::drop-2 (drop-2 inv)
     ::drop-3 (drop-3 inv)}))

(s/fdef chord-melody-voicings
        :args ::chord-melody-args
        :ret (s/keys :req [::closed ::drop-2 ::drop-3])
        :fn #(= (-> % :args :melody)
                (-> % :ret ::closed last)
                (-> % :ret ::drop-2 last)
                (-> % :ret ::drop-3 last)))
