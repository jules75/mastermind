(ns mastermind.core
  (:require [cljs.reader :refer [read-string]]
            [clojure.set :refer [intersection]]
            [dommy.core :as d]
            goog.string.format))

(enable-console-print!)


; mutable state
(def computer-row (atom []))
(def player-guesses (atom []))


(def colours [:red :green :blue :black :white :yellow])


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


(defn random-row
  []
  (vec (repeatedly 4 #(rand-nth colours))))


(defn row->html
  "Turn row (and scores) into html"
  [[a b c d] black-score white-score]
  (goog.string.format 
   "<p>
<span class=\"score black\">%d</span>
<span class=\"score white\">%d</span>
<span class=\"peg %s\">%s</span>
<span class=\"peg %s\">%s</span>
<span class=\"peg %s\">%s</span>
<span class=\"peg %s\">%s</span>
</p>" 
   black-score white-score
   a a b b c c d d))


(defn guess
  [s]
  (let [charmap {\b :blue
                 \r :red
                 \g :green
                 \y :yellow
                 \w :white
                 \k :black}]
    (swap! player-guesses conj (map charmap s))
    (d/set-html! 
     (d/sel1 :#result)
     (apply str
            (for [row @player-guesses]
              (row->html
               (map name row)
               (score-black row @computer-row)
               (score-white row @computer-row)
               ))))))



(reset! computer-row (random-row))

