(ns mastermind.core
  (:require [cljs.reader :refer [read-string]]
            [clojure.set :refer [intersection]]
            [dommy.core :as d]
            goog.string.format))

(enable-console-print!)


; mutable state
(def computer-row (atom []))
(def player-guesses (atom []))


(def charmap {\b :blue
              \r :red
              \g :green
              \y :yellow
              \w :white
              \k :black})


(def colours (vals charmap))


(def possible-rows
  (for [a colours
        b colours
        c colours
        d colours]
    [a b c d]))


(defn transpose
  "Turn rows into columns and vice versa."
  [colls]
  (apply mapv vector colls))


(defn remove-identicals
  "Return rows with any identicals (i.e. same value in same position 
  in both rows) removed. Expects 2 rows."
  [rows]
  (->> rows
       (apply interleave)
       (partition 2)
       (remove #(apply = %))
       transpose))


(defn in-both
  "Returns frequency map of items that appear in both collections.
  Unlike set/intersection, handles duplicates."
  [colls]
  (let [freqs (map frequencies colls)
        ks (apply intersection (map set (map keys freqs)))]
    (select-keys (apply merge-with min freqs) ks)))


(defn score-black
  "Returns number of items in both rows in same position"
  [row1 row2]
  (count (filter true? (map = row1 row2))))


(defn score-white
  "Returns number of items shared by both rows NOT in same position"
  [row1 row2]
  (->> [row1 row2] remove-identicals in-both vals (reduce +)))


(def score (juxt score-black score-white))


(defn random-row
  []
  (vec (repeatedly 4 #(rand-nth colours))))


(defn candidates
  "Return only rows that match the supplied guess/score pairs.
  Used to eliminate impossible row combinations."
  [rows guesses guess-scores]
  (let [match? #(= (score % (first guesses)) (first guess-scores))]
    (if (seq guesses)
      (recur (filter match? rows) (rest guesses) (rest guess-scores))
      rows)))


(defn row->html
  "Turn row (and scores) into html"
  [[a b c d] [black-score white-score]]
  (goog.string.format 
   "<p class=\"%s\">
<span class=\"score black\">%d</span>
<span class=\"score white\">%d</span>
<span class=\"peg %s\">%s</span>
<span class=\"peg %s\">%s</span>
<span class=\"peg %s\">%s</span>
<span class=\"peg %s\">%s</span>
</p>" 
   (if (= 4 black-score) "victory" "")
   black-score white-score
   a a b b c c d d))


(defn suggest
  "Suggests a move/guess"
  []
  (let [scores (map #(score @computer-row %) @player-guesses)
        c (candidates possible-rows @player-guesses scores)]
    (println (str "Possible solutions: " (count c)))
    (d/set-text! 
     (d/sel1 :#hint)
     (str "Suggestion " (rand-nth c)))))


(defn guess
  [e]
  (let [s (d/value (d/sel1 :#guess))]
    (swap! player-guesses conj (map charmap s))
    (d/set-html! 
     (d/sel1 :#result)
     (apply str
            (for [row @player-guesses]
              (row->html
               (map name row)
               (score row @computer-row)
               ))))
    (d/set-value! (d/sel1 :#guess) nil)
    (suggest)
    (.preventDefault e)))


(defn colour-pick
  "Populate guess input when user clicks coloured box."
  [e]
  (let [c (d/attr (.-target e) "data-key")
        el (d/sel1 :#guess)
        guess-so-far (d/value el)]
    (d/set-value! el (str guess-so-far c))))


; init
(reset! computer-row (random-row))
(d/listen! (d/sel1 :button) :click guess)
(doseq [li (d/sel [:#palette :li])]
  (d/listen! li :click colour-pick))
(suggest)

