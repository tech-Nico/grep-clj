(ns Grep.core
  (:import (java.io BufferedReader FileReader File FileNotFoundException))
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
             (-> line
                StringBuffer.
                (. insert s "<*")
                (. insert (+ e 2) "*>")
                (. toString)
            )
                
            )  line matches-map))

(defn show-results [filename results]

  (let [occ  (count-occurrencies results)]
    (if (> occ 0)
     (do
       (println "File: " filename)
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
(defn grep [file search-for]
  "Check if the given filename exists and call the function to
search for the matches. It has side effects since it's printing
an error message if the file doesn't exist. Maybe it should return a value
or throw an exception."
  
  (with-open [rdr (BufferedReader. (FileReader. (. file getCanonicalPath)))]

      ;;This should be refactored by using the -> (or ->> ?) macro
      (show-results
       (. file getCanonicalPath) 
       (sort-by :line-num
                (filter #(> (% :found) 0)
                        (map-indexed  (fn [line-num line]
                                (if (not (nil? line))
                                  (search-line line search-for (+ 1 line-num)))) (line-seq rdr)))))

            ))

(defn as-file [s]
  "Code courtesy of bpsm (https://gist.github.com/bpsm)
Given a filename/path return its java.io.File representation."
  (cond (instance? File s) s   ; already a file, return unchanged
        (string? s) (File. s)  ; return java.io.File for path s
        :else nil))

(defn walk [^File dir]
  "Source code courtesy of bpsm (https://gist.github.com/bpsm)
Given a directory, returns a map with the list of files traversing all subdirectories."
  (let [children (.listFiles dir)
        subdirs (filter #(.isDirectory %) children)
        files (filter #(.isFile %) children)]
    (concat files (mapcat walk subdirs))))


(defn grep-recursive [dir pattern]
  (print "Start grep-recursive")
  (dorun 
      (map #(grep %1 pattern) (walk dir))) 
 )

(defn -main
  ([] (println "Syntax: grep [filename|dir] reg-exp\n"
               "filename|dir    Specify the filename to search into\n"
               "                If this represents a directory, grep will search\n"
               "                in any single file and will traverse directories recursively"
               "reg-exp         The regular expression to search for"))
  ([param]
     (-main))
  ([filename pattern]
     (if (. (as-file filename) isDirectory)
       (grep-recursive (as-file filename) pattern)
       (grep (as-file filename) pattern)
      ))
  ([option dir filename pattern args &]
     (println "Invalid number of arguments")
     (-main)
     )  
  )