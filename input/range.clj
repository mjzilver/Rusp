; Check if a number is between 1 and 10
(defn check-range [n]
  (if (< n 1)
    "below 1"
    (if (> n 10)
      "above 10"
      "within range")))

(print (check-range 0))   ; Should print below 1
(print (check-range 5))   ; Should print within range
(print (check-range 15))  ; Should print above 10
