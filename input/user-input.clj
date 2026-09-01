(defn ask-for-input []
  (print "Please enter some input: ")
  (let [user-input (read-line)]
    (print (concat "You entered: " user-input))))

(ask-for-input)