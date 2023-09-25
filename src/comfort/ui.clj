(ns comfort.ui
  "Simple no-dependency Swing vis infra.
   No GUI interactivity yet..."
  (:import (java.time LocalDate LocalDateTime)
           (java.time.temporal ChronoUnit)
           (java.time.format DateTimeFormatter)
           (javax.swing JFrame JPanel JComponent)
           (java.awt
             Frame
             Graphics Graphics2D
             Dimension
             Color BasicStroke
             Rectangle)
           (java.awt.event
             WindowStateListener
             WindowEvent
             ComponentListener)))

(defn painter
  "Example painter method."
  [^JComponent c ^Graphics2D g]
  (.setBackground g Color/BLUE)
  (.clearRect g 0 0 (/ (.getWidth c) 2) (.getHeight c)))

(defn frame
  "Make a frame which draws its panel using `painter`, which is stored
   internally and is updatable using the returned fn."
  [painter]
  (let [painter* (atom painter)
        #_#__ (add-watch painter* :watch (fn [k r o n]
                                       (println "updated" r "on" k)))
        p (doto (proxy [JPanel] []
                  (paint [g]
                    (@painter* this ^Graphics2D g)))
            (.setPreferredSize (Dimension. 800 600)))
        f (doto (JFrame.)
            (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
            (.add p)
            (.pack)
            (.setLocationRelativeTo nil)
            #_(.addWindowStateListener
              (reify WindowStateListener
                (windowStateChanged [this e]
                  (condp = e
                    WindowEvent/WINDOW_CLOSING
                    (remove-watch painter* :refresh)
                    nil))))
            (.setVisible true)
            (.setResizable true)
            #_(.addComponentListener
              (reify ComponentListener
                (componentResized [self e]
                  #_(println "resized" e)
                  #_(println (.getWidth (first (.getComponents (.getComponent e)))))
                  )
                (componentMoved [self e])
                (componentShown [self e])
                (componentHidden [self e]))))]
    (fn reset-painter [painter]
      #_(println "updating" f "with" painter)
      (reset! painter* painter)
      ;; FIXME Only seems to repaint the first frame if multiple frames use same painter.
      ;; (But why would anyone want that?)
      ;; (Subsqeuent repaints work.)
      (.update f (.getGraphics f)))))

(defmacro repl-frame
  "Make a frame which draws its panel using `painter`, which is a symbol representing
   a var containing a fn. The frame redraws when the var is redefined."
  [painter]
  `(let [ret# (frame ~painter)
         key# (keyword (gensym))]
     (add-watch #'~painter key#
       (fn ~'watch-painter ~'[k r o n]
         (println "watching" #'~painter "on" key#)
         (ret# ~painter)))
     (fn ~'unwatch-painter ~'[]
       (println "unwatching" #'~painter "on" key#)
       (remove-watch #'~painter key#))))

(comment
  (macroexpand-1 '(repl-frame hmm))
  ;; => (clojure.core/let
  ;;     [ret__65896__auto__ (comfort.ui/frame hmm)]
  ;;     (clojure.core/add-watch
  ;;      #'hmm
  ;;      :refresh
  ;;      (clojure.core/fn [k r o n] (println k r) (ret__65896__auto__ hmm)))
  ;;     ret__65896__auto__)
  (def closer (repl-frame painter))
  (closer)
  (.getWatches #'painter)
  (doseq [[k w] (.getWatches #'painter)] (remove-watch #'painter k))
  )
