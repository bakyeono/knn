;;;; k-NN 알고리즘 연습
;;;; 기계학습 세미나 발표용
;;;; author: Bak Yeon O (bakyeono@gmail.com)
;;;; website: http://bakyeono.net
;;;; source: https://github.com/bakyeono/knn

(ns bakyeono.knn
  (:require [clojure.pprint :as pprint]
            [clojure.repl :as repl]))

(defn dist-by-order-p
  "p 도수에 따른 거리 계산기 생성기
  p 도수를 적용하여 거리를 계산하는 함수를 반환한다.
  order-p = 1: Manhattan distance
  order-p = 2: Euclidean distance
  예:
  ((order-p-dist 1) [1 4] [2 3])   -> 2.0"
  [order-p]
  (fn [p q]
    (Math/pow (reduce +
                      (map #(Math/pow (Math/abs (- % %2))
                                      order-p)
                           p
                           q))
              (/ 1 order-p))))

(def manhat-dist (dist-by-order-p 1))
(def euclid-dist (dist-by-order-p 2))

(defn normalize
  "시퀀스의 값을 0-1 사이의 값으로 정규화
  예:
  (normalize [1 2 3 4 5])      -> (0 1/4 1/2 3/4 1)
  (normalize [0 9 18 27 36])   -> (0 1/4 1/2 3/4 1)"
  [x]
  (let [minx (reduce min x)
        maxx (reduce max x)]
    (map #(/ (- % minx)
             (- maxx minx))
         x)))

(defn select-vals
  "키 시퀀스와 해시맵을 입력받아 키에 해당하는 값들을 시퀀스로 반환한다.
  예: (select-vals [:a :c] {:a 10, :b 20, :c 30}) -> (10 30)"
  [ks hashmap]
  (map #(get hashmap %) ks))

(defn sort-hashmaps-by-dist
  "해시맵들을 기준 해시맵과의 거리에 따라 오름차순 정렬한다.
  disf-f: 거리 계산 함수
  by-keys: 거리 계산에 적용할 값들의 키들
  p-hashmap: 기준 해시맵
  q-hashmaps: 정렬할 해시맵들의 시퀀스"
  [dist-f by-keys p-hashmap q-hashmaps]
  (let [dist (fn [q] (dist-f (select-vals by-keys p-hashmap)
                             (select-vals by-keys q)))]
    (sort #(< (dist %) (dist %2))
          q-hashmaps)))

(def training-dataset
  [{ :name "사과",   :sweetness 10, :crispness 9,  :type :fruit     }
   { :name "베이컨", :sweetness 1,  :crispness 4,  :type :protein   }
   { :name "바나나", :sweetness 10, :crispness 1,  :type :fruit     }
   { :name "당근",   :sweetness 7,  :crispness 10, :type :vegetable }
   { :name "샐러리", :sweetness 3,  :crispness 10, :type :vegetable }
   { :name "치즈",   :sweetness 1,  :crispness 1,  :type :protein   }
   { :name "포도",   :sweetness 8,  :crispness 5,  :type :fruit     }
   { :name "강낭콩", :sweetness 3,  :crispness 7,  :type :vegetable }
   { :name "땅콩",   :sweetness 3,  :crispness 6,  :type :protein   }
   { :name "오렌지", :sweetness 7,  :crispness 3,  :type :vegetable }])

(def testing-data { :name "토마토", :sweetness 10, :crispness 10 })

(def result-of-manhat-dist
  (sort-hashmaps-by-dist manhat-dist
                         [:sweetness :crispness]
                         testing-data
                         training-dataset))

(def result-of-euclid-dist
  (sort-hashmaps-by-dist euclid-dist
                         [:sweetness :crispness]
                         testing-data
                         training-dataset))

(defn test-food
  "food의 k-최근접이웃 결과를 계산한다."
  [food k]
  (let [sorted (sort-hashmaps-by-dist euclid-dist
                                 [:sweetness :crispness]
                                 food
                                 training-dataset)
        top-k  (take k sorted)
        types  (map :type top-k)
        freqs  (frequencies types)
        most   (last (sort-by val freqs))]
    most))

