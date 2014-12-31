;;;; "Lucky Bum" card game in Clojure

;;;; Data Definitions

;;;; suit - a symbol; must be in 'suits' vector

;;;; rank - a symbol; must be in 'cards' map

;;; card - vector
;;; 1) rank (:2-:10, :j, :q, :k, :a)
;;; 2) suit (see suits variable below)

;;; deck - vector of cards

;;; hand - map represents each player's cards
;;; :player - specifies player # or name
;;; :down - the down-facing cards, start with 3
;;; :up - the up-facing cards, start with 3
;;; :in-hand - cards in hand, usually 3 or more til source-stack empty

;;; pickup-pile - deck from which cards are distributed to players

;;; discard-pile - deck upon which players play their card, or get cards
;;; from if cannot play (also needs to track how many of top card rank
;;; in a row - i.e. 3 nines or 1 ace on top)

(def suits [:clubs :diamonds :hearts :spades])
(def card-ranks {:3 0 :4 1 :5 2 :6 3 :7 4 :8 5 :9 6 :j 7 :q 8 :k 9 :a 10 :2 11 :10 12})

;; build-deck : -> deck
;; returns shuffled deck of cards
(defn build-deck []
  (let [card-list (keys card-ranks)]
    (loop [s suits, c card-list, deck []]
      (cond (empty? s) (shuffle deck)
            (empty? c) (recur (rest  s) card-list deck)
            :else (recur s (rest c) (conj deck [(first c) (first s)]))))))

;; init-hands : int -> vectorof of hands
;; based on player-count, return an vector of hands; each hand has
;; keys but no values except :player
(defn init-hands [player-count]
  (loop [i 1, hands []]
    (cond (> i player-count) hands
          :else (recur (inc i) (conj hands {:player i :down [] :up [] :in-hand []})))))

;; sort-cards : vectorof cards -> vector of cards
;; sorts cards by rank; by default sorts lowest to highest
(defn sort-cards
  ([crds]
     (sort-cards crds true))
  ([crds asc]
     (let [tmp (sort-by #(card-ranks (first %)) crds)]
       (if asc tmp (reverse tmp)))))

;; deal : int -> vector of hands
;; retuns a vector of hands representing each player's cards
(defn deal [deck player-count]
  (letfn [(init-up-cards [hand]
            (let [sorted (sort-cards (:in-hand hand) false)]
              (assoc hand
                :up (into [] (take 3 sorted))
                :in-hand (into [] (drop 3 sorted)))))]
    (let [deal-order (into [] (concat (repeat 3 :down) (repeat 6 :in-hand)))
          hands (init-hands player-count)]
      (loop [p 0, o deal-order, d deck, h hands]
        (cond (empty? o) (map init-up-cards h)
              (>= p player-count) (recur 0 (rest o) d h)
              :else (recur (inc p) o (rest d)
                           (assoc-in h [p (first o)] (conj (get-in h [p (first o)]) (first d)))))))))

;; matches - rank (vectorof cards) -> vectorof cards
;; return a subset of crds containing all cards that have same rank as
;;rnk
(defn matches [rnk crds]
  (into [] (filter #(= rnk (first %)) crds)))

;; select-cards : rank (vectorof cards) -> (vectorof cards)
;; Based on rank (of top card in discard-pile), return a subset of
;; crds that is the playable cards (i.e. lowest ranking card that
;; meets or beats rnk - return more than one only if more than one of
;; this card); in case of rank 2, return lowest card(s)
(defn select-cards [rnk crds]
  (let [sorted (sort-cards crds)]
    (if (= :2 rnk)
      (matches (ffirst sorted) crds)
      (loop [rm (sort-cards crds)]
        (cond (empty? rm) nil
              (>= (card-ranks (ffirst rm)) (card-ranks rnk)) (matches (ffirst rm) rm)
              :else (recur (rest rm)))))))


;; For testing stuff
(def deck (build-deck))
(def hands (deal deck 2))
(def h (first hands))

;;; Temp Utils
(defn ppd [deck]
  (doseq [d deck]
    (println {:rank (first d), :suit (second d)})))

(defn pph [hands]
  (doseq [h hands]
    (println "Player" (h :player))
    (println "-------------")
    (println "Down: " (h :down))
    (println "Up: " (h :up))
    (println "Hand: " (h :in-hand))
    (println)))
