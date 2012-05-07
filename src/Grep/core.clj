(ns Grep.core
  (:import (java.io BufferedReader FileReader))
  )
(use 'clojure.pprint)

(defn search-line [line search-pattern line-num]
  (let [matcher (re-matcher (re-pattern search-pattern) line)
        results (hash-map :line-num line-num
                          :line line
                          :found 0
                          :occurrencies '())]
    (loop [matched (re-find matcher)
           res results
           acc 0]
      (if (not (nil? matched))
        (do
          (let [new-res res
                new-res (update-in new-res [:found] inc)
                new-res (update-in new-res [:occurrencies] #(conj %1 {:start (.start matcher) :end (.end matcher)} ))
                ]
          (recur (re-find matcher) new-res (inc acc))
          )
         )
        
      
         res
       
      )
    )
  )
)

(defn count-occurrencies [results-map]
     (reduce + (map #(get %1 :found) results-map))
 )

(defn show-results [results]

  (let [occ  (count-occurrencies results)]
    (if (> occ 0)
     (do
        (println "Number of occurrencies found: "  occ)
        (println (map (fn [curr-map]
                        (str "\nLine "
                         (get-in  curr-map [:line-num])
                         "-> "
                         (doseq [found (get-in curr-map [:occurrencies])]
                           
                           (str (get-in found [:start]
                           )
                         )
                        )))
                          results)
        ))
      (println "Sorry. No occurrencies found")
     )
  )
)

 ;; grep simply scan the given fine and search for the specified reg-ex
 ;; It returns a list of couples [col row] where each matching has been found
(defn process-file [filename search-for]
  
    (with-open [rdr (BufferedReader. (FileReader. filename))]

      (loop [ my-line-seq (line-seq rdr)
             line (first my-line-seq)
             line-num 1
             results '()]
        (if-not (nil? line)
          (do
            (let [res (search-line line search-for line-num) ] 
                  
              (recur (rest my-line-seq)
                     (first  my-line-seq)
                     (inc line-num)
                     (if (> (get-in res [:found]) 0)
                       (conj results res)
                       results))
              ))
          (do
            (println "TEST: " results)
            (show-results (sort-by :line-num results))

            )
          
            ))))

(defn grep [filename search-for]
  (if (. (java.io.File. filename) exists)
    (process-file filename search-for)
    (println "File [" filename "] not found")
    )
  )
    
(grep "./test.txt" "let")