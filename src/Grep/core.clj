(ns Grep.core
  (:import (java.io BufferedReader FileReader))
  (:gen-class))
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
      ))))
    
  


(defn count-occurrencies [results-map]
     (reduce + (map #(get %1 :found) results-map))
 )

(defn apply-matches [line matches-map]
  "Given the line and the map containig {:start X :end Y} where a match
has been found in the line, returns the line with the match ***marked***"
  (reduce (fn [line {s :start e :end}] 
            (. (. (. (StringBuffer. line ) insert s "<*") insert (+ e 2) "*>") toString)       
            )  line matches-map))

(defn show-results [results]

  (let [occ  (count-occurrencies results)]
    (if (> occ 0)
     (do
        (println "Number of occurrencies found: "  occ)
        (println (map (fn [curr-map]
                        (let [num-occ (count (get curr-map :occurrencies))]
                          (str "\n"
                               (get-in  curr-map [:line-num])
                               ": "
                                (apply-matches (get-in curr-map [:line]) (get-in curr-map [:occurrencies]))
                                )))
                          results)
        ))
      (println "Sorry. No occurrencies found")
     ))
  )


 ;; grep simply scan the given fine and search for the specified reg-ex
;; It returns a list of couples [col row] where each matching has been found
;; this is a test this is a test this is a test this is a test
(defn process-file [filename search-for]
  
    (with-open [rdr (BufferedReader. (FileReader. filename))]
       
      (show-results
       (sort-by :line-num
                (filter #(> (% :found) 0)
                        (map-indexed  (fn [line-num line]
                                (if (not (nil? line))
                                  (search-line line search-for (+ 1 line-num)))) (line-seq rdr)))))

            ))

(defn grep [filename search-for]
  (if (. (java.io.File. filename) exists)
    (process-file filename search-for)
    (println "File [" filename "] not found")
    )
  )

(defn grep-recursive [pattern]
  (println "TO BE DONE"))

(defn -main
  ([] (println "Syntax: grep [filename|-r] reg-expn\n"
               "-r\t\tSearch all files recursively\n"
               "filename\t\tSpecify the filename to search into\n"
               "reg-exp\t\tThe regular expression to search for"))
  ([filename pattern]
     (if  (= filename "-r")
       (grep-recursive pattern)
       (grep filename pattern)
       ))
  ;;simply ignore any other arguments
  ([filename pattern args &](grep filename pattern )
  ))