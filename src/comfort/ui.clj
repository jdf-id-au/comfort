(ns comfort.ui
  "Simple no-dependency Swing vis infra.
   No GUI interactivity yet..."
  (:require [clojure.java.io :as io])
  (:import (java.time LocalDate LocalDateTime)
           (java.time.temporal ChronoUnit)
           (java.time.format DateTimeFormatter)
           (javax.swing JFrame JPanel JComponent JScrollPane)
           (java.awt
             Frame
             Graphics Graphics2D
             RenderingHints
             Dimension
             Color BasicStroke
             Rectangle)
           (java.awt.event
             WindowStateListener
             WindowEvent
             ComponentListener)
           (java.awt.image BufferedImage)
           (javax.imageio ImageIO)))

(defn painter
  "Example painter method."
  [^JComponent c ^Graphics2D g]
  ;; Can (.setPreferredSize c (Dimension. <width> <height)) for JScrollPane's benefit.
  (.setBackground g Color/YELLOW)
  (.clearRect g 0 0 (/ (.getWidth c) 2) (.getHeight c)))

(defn frame
  "Make a frame which draws its panel using `painter` (within a scroll frame),
  which is stored internally and is updatable using the returned fn.
  Scrolling is unoptimised; fewer redraws when dragging thumb vs gesture/wheel."
  [painter]
  (let [painter* (atom painter)
        p (doto (proxy [JPanel] []
                  (paint [g]
                    (@painter* this ^Graphics2D g)))
            (.setPreferredSize (Dimension. 800 600)))
        f (doto (JFrame.)
            (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
            (.add (JScrollPane. p))
            (.pack)
            (.setLocationRelativeTo nil)
            #_(.addWindowStateListener
              (reify WindowStateListener
                (windowStateChanged [this e]
                  (condp = e
                    WindowEvent/WINDOW_CLOSING
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
    {:reset-painter
     (fn reset-painter [painter]
       (reset! painter* painter)
       ;; FIXME Only seems to repaint the first frame if multiple frames use same painter.
       ;; (But why would anyone want that?)
       ;; (Subsqeuent repaints work.)
       (.update f (.getGraphics f)))
     :repaint (fn repaint [] (.update f (.getGraphics f)))
     :frame f
     :panel p}))

(defn resize
  [{:keys [panel frame]} w h]
  (let [d (Dimension. w h)
        _ (.setPreferredSize panel d)
        d' (.getPreferredSize panel)]
    (when-not (= d d')
      (println "Failed to setPreferredSize" d d')
      (println "min and max: " (.getMinimumSize panel) (.getMaximumSize panel))))
  (.pack frame))

(defn save-png
  [{:keys [panel]} filename]
  (let [p panel
        d (.getPreferredSize p)
        w (.getWidth d)
        h (.getHeight d)
        bi (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
        g (.createGraphics bi)]
    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setBackground g Color/WHITE)
    (.clearRect g 0 0 w h)
    (.paintAll p g)
    (ImageIO/write bi "png" (io/file filename))))

(defmacro repl-frame
  "Make a frame which draws its panel using `painter`, which is a symbol representing
   a var containing a fn. The frame redraws when the var is redefined."
  [painter]
  `(let [f# (frame ~painter)
         k# (keyword (gensym))]
     (println "watching" #'~painter "on" k#)
     (add-watch #'~painter k#
       (fn ~'watch-painter ~'[k r o n]
         ((:reset-painter f#) ~painter)))
     (assoc f#
       ;; TODO could return a :watch-painter
       :unwatch-painter
       (fn ~'unwatch-painter ~'[]
         (println "unwatching" #'~painter "on" k#)
         (remove-watch #'~painter k#)))))

(comment
  (macroexpand-1 '(repl-frame hmm))
  (def f (repl-frame painter))
  ((:unwatch-painter f))
  (.getWatches #'painter)
  (doseq [[k w] (.getWatches #'painter)] (remove-watch #'painter k))
  (map str (ImageIO/getWriterFileSuffixes))
  ;; => ("tif" "jpg" "tiff" "bmp" "gif" "png" "wbmp" "jpeg")
  )
