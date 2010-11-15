(ns ray1729.clojure.markov.core
  (:use [clojure.java.io :only (reader)]
	[clojure.contrib.string :only (join)]))

(defn tokenize-str
  [s]
  "Split the string s into words (strings of non-whitespace characters)"
  (re-seq #"\S+" s))

(defn tokenize-file
  [f]
  "Read the file f and split into words"
  (with-open [r (reader f)]
    (tokenize-str (slurp r))))

(defn build-markov-model
  [tokens prefix-length]
  "Given a collection of tokens, builds a map of prefixes of length prefix-length to suffixes."
  (letfn [(build-model
	   [accum token-groups]
	   (if (seq token-groups)
	     (let [g   (first token-groups)
		   pfx (drop-last g)
		   sfx (last g)]
	       (recur (assoc accum pfx (conj (get accum pfx []) sfx)) (next token-groups)))
	     accum))]
    (build-model {} (partition (inc prefix-length) 1 (seq tokens)))))

(defn build-markov-freq-model
  [tokens prefix-length]
  (letfn [(build-model
	   [accum token-groups]
	   (if token-groups
	     (let [g   (first token-groups)
		   pfx (drop-last g)
		   sfx (last g)
		   f   (get-in accum [pfx sfx] 0)]
	       (recur (assoc-in accum [pfx sfx] (inc f)) (next token-groups)))
	     accum))]
    (build-model {} (seq (partition (inc prefix-length) 1 tokens)))))

(defn rand-freq
  [m]
  (let [t (reduce + (vals m)) r (rand)]
    (loop [ks (keys m) vs (vals m) a 0]
      (when (seq ks)
	(let [next-a (+ a (/ (first vs) t))]
	  (if (>= next-a r)
	    (first ks)
	    (recur (rest ks) (rest vs) next-a)))))))

(defn build-markov-chain
  [model prefix]
  "Generate a lazy-seq of tokens, starting with prefix, using the Markov model
  to determine the next token"
  (if-let [suffixes (get model prefix)]
    (let [next-word (rand-nth suffixes)]
      (cons (first prefix) (lazy-seq (build-markov-chain model (concat (rest prefix) [next-word])))))
    prefix))

(defn markov 
  "Generate a random chain of tokens using a Markov chain algorithm with the
  specified prefix length"
  ([tokens prefix-length chain-length seed]    
     (let [model (build-markov-model tokens prefix-length)
	   chain (build-markov-chain model seed)]
       (join " " (take chain-length chain))))
  ([tokens prefix-length chain-length]
     (markov tokens prefix-length chain-length (take prefix-length tokens))))

(comment

  (defn tokenize-line [s]
    "Split a line into word characters and append a trailing newline to the
    sequence"
    (when (seq s)
      (re-seq #"[\w\n]" (str s "\n"))))

  (defn tokenize-file2 [f]
    "Split lines that begin with a lower-case letter into their component letters"
    (with-open [r (reader f)]
      (doall (apply concat (map tokenize-line (filter #(re-seq #"^[a-z]" %) (line-seq r)))))))
  
  ;; Build a 3-letter prefix map of /usr/share/dict-words
  (def m (build-markov-model (tokenize-file2 "/usr/share/dict/words") 3))
  
  ;; Generate a random "word" using the prefix map
  (apply str (take-while (partial not= "\n") (build-markov-chain m ["a" "b" "b"]))))
