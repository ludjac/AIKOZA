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

(def population ['(- (+ x 1) 0)
                 '(+ 1 (* x x))
                 '(+ 2 0)
                 '(* x (- -1 -2))])

(defn tfun [expr]
  (eval 
    (concat 
      '(fn [x]) [expr])))

(defn goalf [x]
  (+ (* x x) x 1))

(defn fitness [ind goal]
  (let [r (range -1 1 0.1)]
    (/ (reduce + 
         (map #(* % %) 
           (map - 
             (map goal r) (map ind r))))
       (count r))))

(defn ptrue [x]
  (>= x (rand)))

(defn zipsub [expr]
  (loop [loc (z/seq-zip (seq expr)) r '() c 1]
    (if (z/end? loc)
      r
      (recur (z/next loc)
             (concat r
                     (if (and (not (z/branch? loc)) (not (leftm? loc)))
                       [c]))
             (inc c)))))

(defn zipsub2 [expr]
  (next (loop [loc (z/seq-zip (seq expr)) r '() c 1]
    (if (z/end? loc)
      r
      (recur (z/next loc)
             (concat r
                     (if (z/branch? loc)
                       [c]))
             (inc c))))))

(defn leftm? [node]
  (empty? ((node 1) :l)))

(defn hej (prn "Hej jonatan"))