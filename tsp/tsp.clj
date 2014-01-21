; Traveling Salesman Problem
;
; Heuristic solution that builds a route by adding one city at a time such that the insertion
; point of a city is chosen to minimize the distance
;
; This is much smaller, but also much, much slower than the same solution implemented in Java.
; I'm probably doing something in a dumb way.

; Yes, there are CSV libraries that exist, but I did this as an exercise
; This gets all of the costs as a 2D list
(defn parse-city-data [csv]
	(def row-strings (clojure.string/split csv #"\n"))
	(map (comp (partial map #(Integer/parseInt %1)) #(clojure.string/split %1 #",")) row-strings))

; Interface for getting information about the cities to visit
(defprotocol City-Database
	(city-count [this])
	(travel-cost [this from to] (nth (nth city-data from) to)))

; Implementation of the interface that uses a 2D list parsed from a CSV
(defrecord List-City-Database [list-city-data]
	City-Database
	(city-count [this] (count list-city-data))
	(travel-cost [this from to] (nth (nth list-city-data from) to)))

; Convenience function for parsing a CSV and creating a database
(defn city-database-from-csv [csv]
	(->List-City-Database (parse-city-data csv)))

; Calculate the total distance of a route
(defn route-cost [city-database route]
	(def legs (map vector route (drop 1 route)))
	(def distances (map #(apply travel-cost city-database %1) legs))
	(apply + distances))

; Select the lowest cost route
(defn lowest-cost-route [city-database routes]
	(apply (partial min-key (partial route-cost city-database)) routes))

; Heuristically builds a route
; @note This is non-deterministic
(defn build-route [city-database]
	(defn insert-city [route city index]
		(concat (take index route) [city] (drop index route)))

	(defn insert-optimally [route city]
		(def insertion-indices (range 1 (count route)))
		(def possible-routes (map #(insert-city route city %1) insertion-indices))
		(lowest-cost-route city-database possible-routes))

	(defn random-insertion-order []
		(def to-visit (drop 1 (range (city-count city-database))))
		(shuffle to-visit))

	(reduce #(insert-optimally %1 %2) [0 0] (random-insertion-order)))

(def lcd (city-database-from-csv (slurp "./city-data")))

(def routes (repeatedly 100 (partial build-route lcd)))
(def best-route (lowest-cost-route lcd routes))
(println (route-cost lcd best-route) ": " best-route)
