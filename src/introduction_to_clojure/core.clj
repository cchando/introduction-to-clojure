
(ns introduction-to-clojure.core
  (:require [bakery.core :refer :all]))

(defn add-egg []
  (grab :egg)
  (squeeze)
  (add-to-bowl))


(defn add-flour []
  (grab :cup)
  (scoop :flour)
  (add-to-bowl)
  (release))


(defn add-milk []
  (grab :cup)
  (scoop :milk)
  (add-to-bowl)
  (release))


(defn add-sugar []
  (grab :cup)
  (scoop :sugar)
  (add-to-bowl)
  (release))


(defn add-butter []
  (grab :butter)
  (add-to-bowl))


(defn scooped? [ingr]
  (cond
    (= ingr :milk)
    true
    (= ingr :sugar)
    true
    :else
    false))



(defn squeezed? [ingr]
  (cond
    (= ingr :egg)
    true
    :else
    false))


(defn simple? [ingr]
  (= ingr :butter))


(defn add-scooped [ingr]
  (if (scooped? ingr)
    (do
      (grab :cup)
      (scoop ingr)
      (add-to-bowl)
      (release))
      (do
        (println ingr "is not a scoop-able ingredient")
        :error)))



(defn add-squeezed [ingr]
  (if (squeezed? ingr)
    (do
      (grab ingr)
      (squeeze)
      (add-to-bowl))
      (do
        (println ingr "is not a squeezable ingredient")
        :error)))


(defn add-simple [ingr]
  (if (simple? ingr)
    (do
      (grab ingr)
      (squeeze)
      (add-to-bowl))
    (do
      (println ingr "is not a simple ingredient")
      :error)))


(defn add [ingr]
  (cond
    (squeezed? ingr)
    (add-squeezed ingr)
    (scooped? ingr)
    (add-scooped ingr)
    (simple? ingr)
    (add-simple ingr)
    :else
    (do
      (println "Unknown ingredient:" ingr)
      :error)))


(defn add-eggs [n]
  (dotimes [e n]
    (add-egg))
  :ok)


(defn add-flour-cups [n]
  (dotimes [e n]
    (add-flour))
  :ok)


(defn add-milk-cups [n]
  (dotimes [e n]
    (add-milk))
  :ok)


(defn add-sugar-cups [n]
  (dotimes [e n]
    (add-sugar))
  :ok)


(defn add-butters [n]
  (dotimes [e n]
    (add-butter))
  :ok)


(defn bake-cake []
  (add-flour-cups 2)
  (add-eggs 2)
  (add-milk-cups 1)
  (add-sugar-cups 1)
  (mix)
  (pour-into-pan)
  (bake-pan 25)
  (cool-pan))

