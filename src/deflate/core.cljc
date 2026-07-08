(ns deflate.core
  "Pure-cljc DEFLATE (RFC 1951) + zlib (RFC 1950) decompressor.

   Zero-dep, no java.util.zip, no external library — everything is Huffman +
   LZ77 over a bit reader. Extracted from kotoba-lang/kasane (ADR-2606272100)
   where it was the single load-bearing codec backing PSD ZIP channels and
   PDF FlateDecode streams; also used by PNG IDAT, ZIP (raw deflate), and
   WOFF1 per-table compression. Part of the kotoba-lang reverse-domain
   standards-substrate initiative (see com-junkawasaki/root 90-docs/adr/
   for the DEFLATE/ZIP/PNG/TIFF/JPEG/OpenType/ISOBMFF/PDF split batch).

   Correctness-first: output is accumulated in a persistent vector
   (back-references read it via nth). Big rasters should be inflated
   lazily/on demand.")

;; RFC 1951 §3.2.5 — length/distance base values + extra bits.
(def ^:private len-base   [3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258])
(def ^:private len-extra  [0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0])
(def ^:private dist-base  [1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049 3073 4097 6145 8193 12289 16385 24577])
(def ^:private dist-extra [0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13])
;; Order in which code-length-code lengths are stored (RFC 1951 §3.2.7).
(def ^:private clc-order  [16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15])

(defn- bitreader [data]
  {:data (vec data) :len (count data) :bytepos (atom 0) :bitpos (atom 0)})

(defn- getbit
  "Next bit, LSB-first within each byte (RFC 1951 §3.1.1)."
  [br]
  (let [bp @(:bytepos br) bi @(:bitpos br)]
    (when (>= bp (:len br)) (throw (ex-info "inflate: bit EOF" {})))
    (let [byte (nth (:data br) bp)
          bit  (bit-and (bit-shift-right byte bi) 1)]
      (if (= bi 7)
        (do (reset! (:bitpos br) 0) (swap! (:bytepos br) inc))
        (reset! (:bitpos br) (inc bi)))
      bit)))

(defn- getbits
  "Read `n` bits as an integer, LSB-first."
  [br n]
  (loop [i 0 acc 0]
    (if (= i n) acc
        (recur (inc i) (bit-or acc (bit-shift-left (getbit br) i))))))

(defn- align-byte! [br]
  (when (pos? @(:bitpos br))
    (reset! (:bitpos br) 0)
    (swap! (:bytepos br) inc)))

(defn- read-byte-aligned! [br]
  (let [bp @(:bytepos br) b (nth (:data br) bp)]
    (swap! (:bytepos br) inc) b))

(defn- build-huffman
  "Build a canonical Huffman decode table from per-symbol code lengths
   (index = symbol, value = bit length, 0 = unused). RFC 1951 §3.2.2."
  [lengths]
  (let [maxlen   (reduce max 0 lengths)
        bl-count (reduce (fn [m l] (if (pos? l) (update m l (fnil inc 0)) m)) {} lengths)
        next-code (loop [len 1 code 0 acc {}]
                    (if (> len maxlen) acc
                        (let [code (bit-shift-left (+ code (get bl-count (dec len) 0)) 1)]
                          (recur (inc len) code (assoc acc len code)))))
        table (loop [sym 0 nc next-code t (transient {})]
                (if (>= sym (count lengths)) (persistent! t)
                    (let [l (nth lengths sym)]
                      (if (pos? l)
                        (recur (inc sym) (update nc l inc) (assoc! t [l (get nc l)] sym))
                        (recur (inc sym) nc t)))))]
    {:table table :maxlen maxlen}))

(defn- decode-sym
  "Decode one symbol. Codes are read MSB-first (RFC 1951 §3.1.1)."
  [br {:keys [table maxlen]}]
  (loop [len 1 code 0]
    (let [code (bit-or (bit-shift-left code 1) (getbit br))]
      (if-let [s (get table [len code])]
        s
        (do (when (>= len maxlen) (throw (ex-info "inflate: bad Huffman code" {:len len})))
            (recur (inc len) code))))))

(def ^:private fixed-lit
  (build-huffman (vec (concat (repeat 144 8) (repeat 112 9) (repeat 24 7) (repeat 8 8)))))
(def ^:private fixed-dist
  (build-huffman (vec (repeat 30 5))))

(defn- read-dynamic-tables [br]
  (let [hlit  (+ 257 (getbits br 5))
        hdist (+ 1   (getbits br 5))
        hclen (+ 4   (getbits br 4))
        cl-lens (loop [i 0 m {}]
                  (if (= i hclen) m
                      (recur (inc i) (assoc m (nth clc-order i) (getbits br 3)))))
        cl-huff (build-huffman (mapv #(get cl-lens % 0) (range 19)))
        total   (+ hlit hdist)
        all-lens (loop [out []]
                   (if (>= (count out) total) out
                       (let [sym (decode-sym br cl-huff)]
                         (cond
                           (< sym 16) (recur (conj out sym))
                           (= sym 16) (let [rep (+ 3 (getbits br 2)) prev (peek out)]
                                        (recur (into out (repeat rep prev))))
                           (= sym 17) (let [rep (+ 3 (getbits br 3))]
                                        (recur (into out (repeat rep 0))))
                           :else      (let [rep (+ 11 (getbits br 7))]
                                        (recur (into out (repeat rep 0))))))))]
    [(build-huffman (subvec all-lens 0 hlit))
     (build-huffman (subvec all-lens hlit total))]))

(defn- inflate-block! [br lit-huff dist-huff out]
  (loop []
    (let [sym (decode-sym br lit-huff)]
      (cond
        (= sym 256) nil                                   ; end of block
        (< sym 256) (do (vswap! out conj sym) (recur))
        :else (let [li       (- sym 257)
                    length   (+ (nth len-base li) (getbits br (nth len-extra li)))
                    dsym     (decode-sym br dist-huff)
                    distance (+ (nth dist-base dsym) (getbits br (nth dist-extra dsym)))]
                (dotimes [_ length]
                  (let [o @out]
                    (vswap! out conj (nth o (- (count o) distance)))))
                (recur))))))

(defn inflate-raw
  "Inflate a raw DEFLATE stream (no zlib header). Returns a vector of bytes."
  [data]
  (let [br  (bitreader data)
        out (volatile! [])]
    (loop []
      (let [final (getbit br)
            btype (getbits br 2)]
        (case btype
          0 (do (align-byte! br)
                (let [b0 (read-byte-aligned! br) b1 (read-byte-aligned! br)]
                  (read-byte-aligned! br) (read-byte-aligned! br)   ; NLEN (ignored)
                  (dotimes [_ (+ b0 (* 256 b1))]
                    (vswap! out conj (read-byte-aligned! br)))))
          1 (inflate-block! br fixed-lit fixed-dist out)
          2 (let [[lit dist] (read-dynamic-tables br)]
              (inflate-block! br lit dist out))
          (throw (ex-info "inflate: reserved block type" {:btype btype})))
        (if (= final 1) @out (recur))))))

(defn inflate
  "Inflate a zlib (RFC 1950) stream. Returns a vector of unsigned bytes."
  [data]
  (let [v     (vec data)
        flg   (nth v 1)
        fdict (bit-test flg 5)
        off   (if fdict 6 2)]                              ; skip 2-byte header (+4 if FDICT)
    (inflate-raw (subvec v off))))
