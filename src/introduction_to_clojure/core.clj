

(ns introduction-to-clojure.core
  (:require [bakery.core :refer :all]))

(def baking  {:recipes {:cake {:ingredients {:egg   2
                                             :flour 2
                                             :sugar 1
                                             :milk  1}
                               :steps [[:add :all]
                                       [:mix]
                                       [:pour]
                                       [:bake 25]
                                       [:cool]]}
                        :cookies {:ingredients {:egg 1
                                                :flour 1
                                                :butter 1
                                                :sugar 1}
                                  :steps [[:add :all]
                                          [:mix]
                                          [:pour]
                                          [:bake 30]
                                          [:cool]]}
                        :brownies {:ingredients {:egg 2
                                                 :flour 2
                                                 :butter 2
                                                 :cocoa 2
                                                 :sugar 1
                                                 :milk 1}
                                   :steps [[:add :butter]
                                           [:add :cocoa]
                                           [:add :sugar]
                                           [:mix]
                                           [:add :egg]
                                           [:add :flour]
                                           [:add :milk]
                                           [:mix]
                                           [:pour]
                                           [:bake 35]
                                           [:cool]]}}})(def pantry-ingredients #{:flour :sugar :cocoa})

(def fridge-ingredients #{:milk :egg :butter})

(def squeezed #{:egg})

(def scooped #{:milk :flour :sugar :cocoa})

(def simple #{:butter})

(def cake-ingredients {:egg 2
                       :flour 2
                       :sugar 1
                       :milk 1})


(def cookie-ingredients {:egg 1
                         :flour 1
                         :sugar 1
                         :butter 1})


(def brownie-ingredients {:egg 1
                         :flour 2
                         :cocoa 2
                         :milk 1
                         :sugar 1
                         :butter 2})

(defn error [& args]
  (apply println args)
  :error)


(defn flag []
  (println "---FLAG---"))


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


(defn perform [ingredients step]
  (cond (= :cool (first step))
        (cool-pan)
        (= :mix (first step))
        (mix)
        (= :pour (first step))
        (pour-into-pan)
        (= :bake (first step))
        (bake-pan (second step))
        (= :add (first step))
        (cond
          (= (count step) 2)
          (if
              (= (second step) :all)
            (doseq [kv ingredients]
              (add (first kv) (second kv)))
            (add (second step) (get ingredients (second step))))
          (= (count step) 3)
          (add (second step) (nth step 2))
          :else
          (error "I don't know how to add" (second step)))
        :else
        (error "I don't know how to" (first step))))


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
    (reduce add-ingredients {}
            (for [kv items]
              (let [recipes (get baking :recipes)
                    recipe (get recipes (first kv))
                    ingredients (get recipe :ingredients)]
                (multiply-ingredients (second kv) ingredients))))))


(defn orders->ingredients [orders]
  (reduce add-ingredients {} (for [order orders]
                               (order->ingredients order))))


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


(defn bake-recipe [recipe]
  (last
   (for [step (get recipe :steps)]
     (perform (get recipe :ingredients) step))))


(defn bake [item]
  (bake-recipe (get (get baking :recipes) item)))

