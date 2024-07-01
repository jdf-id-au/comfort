(ns comfort.ui
  "Simple no-dependency Swing vis infra.
   No GUI interactivity yet..."
  (:require [clojure.java.io :as io])
  (:import (java.time LocalDate LocalDateTime)
           (java.time.temporal ChronoUnit)
           (java.time.format DateTimeFormatter)
           (javax.swing
             JFrame
             JPanel
             JLabel
             JComponent
             JScrollPane
             ImageIcon)
           (java.awt
             Frame
             Graphics Graphics2D
             RenderingHints
             Dimension
             Color BasicStroke
             Rectangle
             GridBagLayout GridBagConstraints
             )
           (java.awt.event
             WindowStateListener
             WindowEvent
             ComponentListener)
           (java.awt.image BufferedImage)
           (javax.imageio ImageIO)))

(defn painter
  "Example painter method."
  [^JComponent c ^Graphics2D g]
  (.setBackground g Color/YELLOW)
  (.clearRect g 0 0 (/ (.getWidth c) 2) (.getHeight c)))

(defn buffer
  "Buffer painting into image for smooth scrolling etc."
  [panel]
  (let [d (.getPreferredSize panel)
        w (.getWidth d)
        h (.getHeight d)
        bi (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
        g (.createGraphics bi)]
    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setBackground g Color/WHITE)
    (.clearRect g 0 0 w h)
    (.paint panel g)
    bi))

(defn frame
  "Make a frame which draws its panel using `painter` (within a scroll frame),
  which is stored internally and is updatable using the returned fn."
  [painter]
  (let [painter* (atom painter)
        p (doto (proxy [JPanel] []
                  (paint [g]
                    (proxy-super paintComponent g)
                    (@painter* this ^Graphics2D g)))
            ;; chicken and egg: can't really set within `painter` because it hasn't painted yet
            (.setPreferredSize (Dimension. 800 600)))
        buffer* (atom (buffer p))
        bp (doto (proxy [JPanel] []
                   (paint [g]
                     (proxy-super paintComponent g)
                     (.drawImage g @buffer* 0 0 this)))
             (.setPreferredSize (.getPreferredSize p)))
        s (JScrollPane. bp)
        hsb (doto (.getHorizontalScrollBar s) (.setUnitIncrement 3))
        vsb (doto (.getVerticalScrollBar s) (.setUnitIncrement 3))
        f (doto (JFrame.)
            (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
            (.add s)
            (.pack)
            #_(.addWindowStateListener
              (reify WindowStateListener
                (windowStateChanged [this e]
                  (condp = e
                    WindowEvent/WINDOW_CLOSING
                    nil))))
            #_(.addComponentListener
              (reify ComponentListener
                (componentResized [self e]
                  #_(println "resized" e)
                  #_(println (.getWidth (first (.getComponents (.getComponent e)))))
                  )
                (componentMoved [self e])
                (componentShown [self e])
                (componentHidden [self e]))))
        max-size #(let [d (.getPreferredSize s)  ; Seems unfair to have to do this
                        i (.getInsets f)]
                    (Dimension. (.getWidth d) (+ (.getHeight d) (.-top i))))]
    (doto f
      (.setMaximumSize (max-size))
      (.setLocationRelativeTo nil)
      (.setVisible true)
      (.setResizable true))
    {:reset-painter
     (fn reset-painter [painter]
       (reset! painter* painter)
       (reset! buffer* (buffer p))
       (.revalidate bp)
       (.repaint bp))
     :repaint (fn repaint []
                (reset! buffer* (buffer p))
                (.repaint bp))
     :resize (fn [w h] (let [d (Dimension. w h)]
                         (.setPreferredSize p d)
                         (.setPreferredSize bp d)
                         (reset! buffer* (buffer p))
                         (.revalidate f)
                         (.repaint f)
                         (.pack f)
                         (.setMaximumSize f (max-size))))
     :save-png (fn [filename] (ImageIO/write @buffer* "png" (io/file filename)))
     }))

(defmacro repl-frame
  "Make a frame which draws its panel using `painter`, which is a symbol bound to
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
