(ns clojure-app.core
  (:gen-class)
  (:require [clojure.math])
  (:require [clojure.string])
  (:require [clojure.stacktrace]))

(def ids-to-marks [" ", "X", "O"])

(defn prompt [text]
  (print text)
  (flush)
  (read-line))

(defn create-board []
  (vec (repeat 3 (vec (repeat 3 0)))))

(defn make-move
  ([board id [row col]]
   (assoc board row (assoc (board row) col id)))
  )

(defn transpose
  [matrix]
  (apply mapv vector matrix)
  )

(defn diagonals
  [board]
  [(mapv #(get-in board [% %]) [0 1 2])
   (mapv #(get-in board [% (- 2 %)]) [0 1 2])
   ])

(defn all-lines
  [board]
  (concat board (transpose board) (diagonals board)))

(defn is-winning-line?
  [line]
  (and (= 1 (count (set line))) (not= 0 (line 1))))

(defn is-winning-board?
  [board]
  (true? (some is-winning-line? (all-lines board))))

(defn has-valid-move?
  [board]
  (some zero? (flatten board)))

(defn is-valid-move?
  [board row col]
  (zero? (get-in board [row col])))

(defn num-to-move
  [num]
  [(int (clojure.math/floor (/ (- num 1) 3)))
   (mod (- num 1) 3)]
  )

(defn valid-moves
  [board]
  (map num-to-move (keep-indexed #(if (zero? %2) (+ %1 1) nil) (flatten board))))

(defn find-forced-square
  [board id]
  (let [opp-id (- 3 id)]
    (first
     (filter
      #(is-winning-board? (make-move board opp-id %))
      (valid-moves board)))))

(defn random-valid-move
  [board]
  (let [moves (valid-moves board)]
    (nth moves (rand-int (count moves)))
    ))

(def bot-id 2)

(defn move-to-num
  [[row col]]
  (+ (* row 3) col 1))

(defn choose-move-for-bot
  [board]
  (or (find-forced-square board bot-id) (random-valid-move board)))

(defn ask-for-move
  [id]
  (prompt (str "Pick a square (1-9) for Player " (ids-to-marks id) ": ")))

(defn choose-move
  [board id]
  (if (= id bot-id)
    (choose-move-for-bot board)
    (let [answer (ask-for-move id)]
      (if-not (re-matches #"^[1-9]$" answer)
        (do
          (println "Invalid move!")
          (choose-move board id))
        (let [num (read-string answer)
              [row col] (num-to-move num)]
          (if-not (is-valid-move? board row col)
            (do
              (println "Invalid move!")
              (choose-move board id))
            [row col]))))))

(defn row-padding [] "    ")

(defn output-for-row
  [row]
  (str " " (clojure.string/join " │ " (map ids-to-marks row))))

(defn output-for-board
  [board]
  (clojure.string/join "\n"
    [
     ""
     ""
     (str (row-padding) "TIC TAC TOE")
     ""
     ""
     (str (row-padding) (output-for-row (board 0)))
     (str (row-padding) "───┼───┼───")
     (str (row-padding) (output-for-row (board 1)))
     (str (row-padding) "───┼───┼───")
     (str (row-padding) (output-for-row (board 2)))
     ""
     ""
    ]))

(defn clear-output [](print (str "\u001Bc")))

(defn output-board [board]
  (clear-output)
  (println (output-for-board board)))

(defn ask-to-play-again
  []
  (prompt (str "Play again? [Y/n]: ")))

(defn choose-to-play-again
  []
  (let [answer (clojure.string/lower-case (ask-to-play-again))]
    (case answer
      "" true
      "y" true
      "n" false
      (choose-to-play-again)
      )
    ))

(defn announce-winner
  [id]
  (println "Player" (ids-to-marks id) "wins!"))

(defn announce-draw
  []
  (println "It's a draw!"))

(defn say-goodbye
  []
  (println "Goodbye!"))

(defn output-bot-move
  [move]
  (println (str "Player " (ids-to-marks bot-id) " picks square " (move-to-num move) "!")))

(defn play
  ([]
   (play (create-board) 1 []))
  ([board next-id move-history]
   (output-board board)
   (if (and (not= next-id bot-id) (seq move-history)) (output-bot-move (last move-history)) nil)
   (if-not (is-winning-board? board)
     (if-not (has-valid-move? board)
       (do
         (announce-draw)
         (if (choose-to-play-again) (play) (say-goodbye)))
       (let [move (choose-move board next-id)]
         (play
          (make-move board next-id move)
          (- 3 next-id)
          (conj move-history move))))
     (do
       (announce-winner (- 3 next-id))
       (if (choose-to-play-again) (play) (say-goodbye))
       ))
     ))

(defn -main
  []
  (try
    (play)
    (catch Exception e (println "Something went wrong!" (and e "")))))