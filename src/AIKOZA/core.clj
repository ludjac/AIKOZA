(ns AIKOZA.core)

(require ['clojure.zip :as 'z])


(defn in? 
  "returns true if ele is in seq"
  [seq ele]
  (some #(= ele %) seq)
  )

(defn rand_func [] (rand-nth '(+ - * div)))

(defn div 
  [up down]
  (if (zero? down)
    0 
    (/ up down))
  )

(defn rand_term [] (rand-nth (list 'x 'y (- (rand-int 10) 5))))

(defn rand_args 
  [func] 
  (if (in? '(+ - *) func)
    (rand-nth '(2 2 2 3 4))
    2
    )
  )

(defn gen_rnd_expr [depth]
  (if (or (zero? depth) (zero? (rand-int 10)))
    (rand_term)
    (let [func (rand_func)
          args (rand_args func)]
      (list func
          (take args (repeatedly #(gen_rnd_expr (dec depth))))
          )
      )
    )
  )