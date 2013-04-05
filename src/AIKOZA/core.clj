(ns AIKOZA.core)
(require ['clojure.zip :as 'z])

(defn in? 
  "returns true if ele is in seq"
  [seq ele]
  (some #(= ele %) seq)
  )

; returnerar en funktion av (+-*/), där div är nollsäker!
(defn rand_func [] (rand-nth '(+ - *)))

; nollsäker division returnerar noll om det är division med noll
(defn d 
  [up down]
  (if (zero? down)
    0 
    (/ up down))
  )

; returnerar random termer ex. x,y och konstanter mellan -5 och 5. 
(defn rand_term [] (rand-nth (list 'x (- (rand-int 10) 5))))

; returnerar antalet argument för olika functioner. 
;Alla utom div är två eller fler, div har alltid två. 
(defn rand_args 
  [func] 
  (if (in? '(+ - *) func)
    (rand-nth '(2 2 2 3 4))
    2
    )
  )

; Genererar random kod. Depth avgör träddjup. 
(defn gen_rnd_expr [depth]
  (if (or (zero? depth))
    (rand_term)
    (let [func (rand_func)
          args (rand_args func)]
      (concat (list func) ;(gen_rnd_expr (dec depth)) (gen_rnd_expr (dec depth))
          (take args (repeatedly #(gen_rnd_expr (dec depth))))
          )
      )
    )
  )

(defn zipmerge 
  [expr n subexpr]
  (loop [loc (z/seq-zip (seq expr)) c 1]
          (if (= c n)
            (z/root (z/replace loc subexpr))
            (recur (z/next loc) (inc c))
          )
  )
)

; Returnerar nil om passande indiviad saknas,
; returnerar bästa individen om det finns fler
(defn finsihedpop [popul limit]
  (let [topind (apply min-key #(fitness (tfun %) goalf) popul)]
    (if (> limit (fitness (tfun topind) goalf))
      topind
      nil
      )
      )
    )


;startpopulationen i kozas artikel
(def population '((- (+ x 1) 0)
                 (+ 1 (* x x))
                 (+ 2 0)
                 (* x (- -1 -2))))

;tar en individ tex (population 1) och returnerar en funktion av den
(defn tfun [expr]
  (eval 
    (concat 
      '(fn [x]) [expr])))


;målfunktionen i kozas artikel
(defn goalf [x]
  (+ (* x x) x 1))

(defn foo [x]
  (loop [l x r '()]
    (if-not (empty? l)
    (if-not (sequential? (last l))
      (recur (drop-last l) (conj r (last l)))
      (recur (concat (drop-last l) (last l)) r ))
    r)))
    

;fitnessfunktion, tar en individ och en målfunktion om returnerar snittet
;av kvadratiska avståndet mellan funktionerna (-1 till 1 med 0.1 som steg)
(defn fitness [ind goal]
  (let [r (range -1 1 0.1)]
    (/ (reduce + 
         (map #(* % %) 
           (map - 
             (map goal r) (map ind r))))
       (count r))))

;returnerar true ibland (tex 90% av fallen om x är 0.9)
(defn ptrue [x]
  (>= x (rand)))

;kollar om en nod i en zipper är längst till vänster i ett träd
;kastar exception om du kollar på en root till ett träd eller subträd
(defn leftm? [node]
  (empty? ((node 1) :l)))

;returnerar hur många gånger man behöver "nexta" ett träd för att hitta de olika löven
(defn zipsub [expr]
  (loop [loc (z/seq-zip (seq expr)) r '() c 1]
    (if (z/end? loc)
      (if (empty? r)
        '(2)
        r)
      (recur (z/next loc)
             (concat r
                     (if (and (not (z/branch? loc)) (not (leftm? loc)))
                       [c]))
             (inc c)))))
;returnerar hur många gånger man behöver "nexta" ett träd för at hita subträden
(defn zipsub2 [expr]
  (next (loop [loc (z/seq-zip (seq expr)) r '() c 0]
    (if (z/end? loc)
      (if (empty? r)
        '(2)
        r)
      (recur (z/next loc)
             (concat r
                     (if (z/branch? loc)
                       [c]))
             (inc c))))))

(defn mrandnth [x]
  (if (empty? x)
    (gen_rnd_expr 1)
    (rand-nth x)))

;tar ett ett slumpnmässigt subträd ur expr, inte root
(defn subtrees [expr]
  (mrandnth
  (filter list? 
  (next (loop [loc (z/seq-zip (seq expr)) r '()]
    (if (z/end? loc)
      r
      (recur (z/next loc)
             (concat r
                     (if (z/branch? loc)
                       (z/node loc))))))))
  ))

;plockar ut den bästa (enligt målfunktionen g) av n stycken slumpmässiga individer ur populationen pop
(defn tournsel [pop n g]
  (first (min-key #(fitness (tfun %) g)
                (repeatedly n #(rand-nth pop)))))



(defn nextgen [pop g n]
  (let [spop (sort-by #(fitness (tfun %) g) pop) r (take n spop) k (- (count pop) n)]
    (concat r (repeatedly k #(let [i (tournsel pop 10 g) nn (rand-nth (zipsub i))]
                     (zipmerge i nn (subtrees (tournsel pop 10 g))))))))
    

; generate population of size size and depth 2 or 3. 
(defn gen_population 
  [size]
  (take size (repeatedly #(gen_rnd_expr (rand-nth '(2 3)))))
  )

(defn bestind [pop]
  (apply min (map #(fitness (tfun %) goalf) pop)))

(defn bestf [pop]
  (apply min-key #(fitness (tfun %) goalf) pop))

(use '(incanter core stats charts))
; Histogrammar fitness från en populaiton. 
; Svårt att få det snyggt för skillnaderna inom generationen är så stora. 
; ex. max 1e40 min 0.66. Kör därför log på datan.
; Tycker det blir lite konstiga grupperingar men det 
; kanske finns en anledning till det!? ideér? 
; www.incanter.com  (Data sorcery) :)
; Glöm inte (use 'incanter... ovan!  
(defn pophist [popul] 
  (view (histogram (let [data (filter #(if (> % 100) 100 %) (doall (map #(fitness (tfun %) goalf) popul)))] 
                     (prn data) 
                     (log data)) :nbins 100)
        )
  )

(defn plot [fun g]
  (view (doto (function-plot g -1 1) (add-function fun -1 1)))
  )

(def gplot (box-plot []))

(defn -main [spop]
  (view gplot)
  (loop [cpop spop c 0]
    (if-let [r (or (finsihedpop cpop 0.01) (> c 50))]
      (do (println c) (plot (tfun (bestf cpop)) goalf) (bestf cpop))
      (do (println c) (println (bestind cpop)) (add-box-plot gplot (map #(fitness (tfun %) goalf) cpop)) (recur (nextgen cpop goalf 3) (inc c))))))


