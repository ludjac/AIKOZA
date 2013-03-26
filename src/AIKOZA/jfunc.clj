(ns jfunc)
(require ['clojure.zip :as 'z])



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

(defn listofsub [expr]
  (if-not (empty? expr)
    (concat 
      [expr] 
      (filter #(not (list? %)) (next expr))
      (map listofsub (filter list? expr)))
  nil))

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
  


