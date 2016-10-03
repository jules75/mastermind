(ns mastermind.core
  (:require [clojure.set :refer [intersection]]))

(enable-console-print!)

(def colours #{:red :green :blue :black :white :yellow})


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


(def possible-rows
  (for [a colours
        b colours
        c colours
        d colours]
    [a b c d]))


(let [target [:red :white :blue :red]]
  (doseq [row possible-rows]
    (println 
     (score-black row target) 
     (score-white row target)
     row)))


;(println (score-white [:red :white :blue :green] [:white :yellow :white :yellow]))
;(println (score-white [1 2 3 4] [2 5 2 5]))
;(println (score-white [1 2 3 4] [4 2 1 3]))

;(println (in-both [[:red :green :red] [:blue :red :yellow]]))
;(println (in-both [[:red :green :red] [:green :red :yellow]]))
;(println (in-both [[:red :green :blue] [:blue :red :red]]))
;(println (in-both [[:red :green :red] [:green :red :red]]))
;(println (in-both [[:blue :white :black] [:yellow :red :green]]))


