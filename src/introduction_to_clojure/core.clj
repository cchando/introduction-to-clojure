
(ns introduction-to-clojure.core
  (:require [bakery.core :refer :all]))

(def pantry-ingredients #{:flour :sugar})

(def fridge-ingredients #{:milk :egg :butter})

(def squeezed #{:egg})

(def scooped #{:milk :flour :sugar})

(def simple #{:butter})

(def cake-ingredients {:egg 2
                       :flour 2
                       :sugar 1
                       :milk 1})

(def cookie-ingredients {:egg 1
                         :flour 1
                         :sugar 1
                         :butter 1})

(defn error [& args]
  (apply println args)
  :error)

(defn scooped? [ingr]
  (contains? scooped ingr))


(defn squeezed? [ingr]
  (contains? squeezed ingr))

(defn simple? [ingr]
  (contains? simple ingr))


(defn add-scooped
  ([ingr n]
   (if (scooped? ingr)
     (dotimes [i n]
       (grab :cup)
       (scoop ingr)
       (add-to-bowl)
       (release))
     (error ingr "is not a scoop-able ingredient")))
  ([ingr]
   (add-scooped ingr 1)))

(defn add-squeezed
  ([ingr n]
    (if (squeezed? ingr)
      (dotimes [i n]
        (grab ingr)
        (squeeze)
        (add-to-bowl))
      (error ingr "is not a squeezable ingredient")))
  ([ingr]
    (add-squeezed ingr 1)))


(defn add-simple
  ([ingr n]
    (if (simple? ingr)
     (dotimes [i n]
       (grab ingr)
       (add-to-bowl))
     (error ingr "is not a simple ingredient")))
  ([ingr]
   (add-simple ingr 1)))


(defn add
  ([ingr n]
    (cond
      (squeezed? ingr)
      (add-squeezed ingr n)
      (scooped? ingr)
      (add-scooped ingr n)
      (simple? ingr)
      (add-simple ingr n)
      :else
      (error "Unknown ingredient:" ingr)))
  ([ingr]
    (add ingr 1)))


(defn bake-cake []
  (add :flour 2)
  (add :egg 2)
  (add :milk 1)
  (add :sugar 1)
  (mix)
  (pour-into-pan)
  (bake-pan 25)
  (cool-pan))


(defn bake-cookies []
  (add :egg 1)
  (add :flour 1)
  (add :sugar 1)
  (add :butter 1)
  (mix)
  (pour-into-pan)
  (bake-pan 30)
  (cool-pan))


(defn from-pantry? [ingr]
  (contains? pantry-ingredients ingr))


(defn from-fridge? [ingr]
  (contains? fridge-ingredients ingr))


(defn fetch-pantry
  ([ingr]
   (fetch-pantry ingr 1))
  ([ingr n]
   (if (from-pantry? ingr)
     (do
       (go-to :pantry)
       (dotimes [i n]
         (load-up ingr))
       (go-to :prep-area)
       (dotimes [i n]
         (unload ingr)))
     (error ingr "not from pantry"))))


(defn fetch-fridge
  ([ingr]
   (fetch-fridge ingr 1))
  ([ingr n]
   (if (from-fridge? ingr)
     (do
       (go-to :fridge)
       (dotimes [i n]
         (load-up ingr))
       (go-to :prep-area)
       (dotimes [i n]
         (unload ingr)))
     (error ingr "not from fridge"))))


(defn fetch
  ([ingr]
   (fetch ingr 1))
  ([ingr n]
   (cond
     (from-fridge? ingr)
     (fetch-fridge ingr n)
     ;;
     (from-pantry? ingr)
     (fetch-pantry ingr n)
     ;;
     :else
     (println "I don't know where to find ingr"))))


(defn load-up-amount [ingr amount]
  (dotimes [i amount]
    (load-up ingr)))


(defn unload-amount [ingr amount]
  (dotimes [i amount]
    (unload ingr)))


(def locations {:pantry pantry-ingredients
                :fridge fridge-ingredients})


(defn fetch-list [shopping-list]
  (doseq [location (keys locations)]
    (go-to location)
    (doseq [ingr (get locations location)]
      (load-up-amount ingr (get shopping-list ingr 0)))
    (go-to :prep-area)
    (doseq [ingr (get locations location)]
      (unload-amount ingr (get shopping-list ingr 0)))))


(defn add-ingredients [a b]
  (merge-with + a b))


(defn multiply-ingredients [n list]
  (into {}
    (for [kv list]
      [(first kv) (* n (second kv))])))


(defn order->ingredients [order]
  (let [items (get order :items)]
    (add-ingredients
     (multiply-ingredients (get items :cake 0) cake-ingredients)
     (multiply-ingredients (get items :cookie 0) cookie-ingredients))))


(defn orders->ingredients [orders]
  (reduce add-ingredients {} (for [order orders]
                               (order->ingredients order))))


(defn day-at-the-bakery []
  (let [orders (get-morning-orders)
        ingredients (orders->ingredients orders)]
    (fetch-list ingredients)
    (doseq [order orders]
      (let [items (get order :items)]
        (dotimes [n (get items :cake 0)]
          (let [rack-id (bake-cake)]
            (delivery {:orderid (get order :orderid)
                       :address (get order :address)
                       :rackids [rack-id]})))
        (dotimes [n (get items :cookie 0)]
          (let [rack-id (bake-cookies)]
            (delivery {:orderid (get order :orderid)
                       :address (get order :address)
                       :rackids [rack-id]})))))))


(defn bake [item]
  (cond
    (= item :cake)
    (bake-cake)
    (= item :cookies)
    (bake-cookies)
    :else
    (error "I don't know how to bake" item)))


(defn day-at-the-bakery []
  (let [orders (get-morning-orders)
        ingredients (orders->ingredients orders)]
    (fetch-list ingredients)
    (doseq [order orders]
      (let [items (get order :items)
            racks (for [kv items
                        i (range (second kv))]
                    (bake (first kv)))]
        (delivery {:orderid (get order :orderid)
                   :address (get order :address)
                   :rackids racks})))))


(defn main []
  (day-at-the-bakery))


