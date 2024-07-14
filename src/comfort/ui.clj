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
             JComponent
             JScrollPane)
           (java.awt
             Frame
             Graphics Graphics2D
             RenderingHints
             Point Dimension Rectangle
             Color BasicStroke)
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
  "Make a frame which draws its panel using `painter` (buffered and within a scroll frame),
  which is stored internally and is updatable using the returned fn."
  [painter w h]
  (let [painter* (atom painter)
        p (doto (proxy [JPanel] []
                  (paint [g]
                    (proxy-super paintComponent g)
                    (@painter* this ^Graphics2D g)))
            ;; chicken and egg: can't really set within `painter` because it hasn't painted yet
            (.setPreferredSize (Dimension. w h)))
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
        max-size #(let [d (.getPreferredSize s) ; seems unfair to have to do this
                        i (.getInsets f)] ; only top is non-zero on macOS
                    (Dimension.
                      (+ (.getWidth d) (.-left i) (.-right i))
                      (+ (.getHeight d) (.-top i) (.-bottom i))))]
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
     :frame f
     :save-png (fn [filename] (ImageIO/write @buffer* "png" (io/file filename)))}))

(defmacro with-graphics
  "Push a copy of graphics context (must be `g`), do body and pop context."
  [& body]
  `(let [~'g (.create ~'g)] ; deliberately unhygienic!
     ~@body
     (.dispose ~'g)))

(defmacro repl-frame
  "Make a frame which draws its panel using `painter`, which is a symbol naming
  a var bound to a fn. The frame redraws when the var is rebound."
  ([painter] `(repl-frame ~painter 800 600))
  ([painter w h]
   `(let [f# (frame ~painter ~w ~h)
          k# (keyword (gensym))
          u# (fn ~'unwatch-painter ~'[]
               ;;(println "unwatching" #'~painter "on" k#)
               (remove-watch #'~painter k#))]
      ;;(println "watching" #'~painter "on" k#)
      (add-watch #'~painter k#
        (fn ~'watch-painter ~'[k r o n]
          ((:reset-painter f#) ~painter)))
      (assoc f#
        ;;:unwatch-painter u#
        :close (fn ~'close ~'[]
                 (u#)
                 (.dispatchEvent (:frame f#) (WindowEvent. (:frame f#) WindowEvent/WINDOW_CLOSING)))))))

(defprotocol Sized
  "Integers"
  (w [this])
  (h [this]))

(defprotocol Located
  "Integers"
  (x [this])
  (y [this]))

(defprotocol Change
  "Integers"
  (inset [this by])
  (align [this within how]))

(defrecord Margin [t r b l]
  Sized
  (w [_] (- r l))
  (h [_] (- b t))
  Located
  (x [_] l)
  (y [_] t))

(defn -inset [o by]
  (cond (int? by) (doto o
                    (.translate by by)
                    (.setSize (- (w o) (* 2 by)) (- (h o) (* 2 by)))))
  (instance? Margin by) (doto o
                          (.translate (:l by) (:t by))
                          (.setSize (- (w o) (:l by) (:r by))
                            (- (h o) (:t by) (:b by)))))

(defn -align [o within how]
  (case how
    :top-right (inset within (Margin. 0 0 (- (h within) (h o))
                               (- (w within) (w o))))))

(extend-type Dimension
  Sized
  (w [this] (.width this))
  (h [this] (.height this))
  Change
  (inset [this by] (-inset (Rectangle. (Point. 0 0) this) by))
  (align [this within how] (-align this within how)))

(extend-type Point
  Sized
  (w [_] 0)
  (h [_] 0)
  Located
  (x [this] (.x this))
  (y [this] (.y this)))

(extend-type java.awt.geom.Rectangle2D$Float ; gross
  Sized
  (w [this] (int (.getWidth this)))
  (h [this] (int (.getHeight this)))
  Located
  (x [this] (int (.getX this)))
  (y [this] (int (.getY this)))
  Change
  (inset [this by] (-inset (Rectangle. this) by))
  (align [this within how] (-align this within how)))

(extend-type Rectangle
  Sized
  (w [this] (int (.getWidth this)))
  (h [this] (int (.getHeight this)))
  Located
  (x [this] (int (.getX this)))
  (y [this] (int (.getY this)))
  Change
  (inset [this by] (-inset (Rectangle. this) by))
  (align [this within how] (-align this within how)))

(defn string-bounds [g s]
  (let [s (str s)
        fm (.getFontMetrics g)
        b (.getStringBounds fm s g)]
    [s b]))

(comment
  (macroexpand-1 '(repl-frame hmm))
  (def f (repl-frame painter))
  (.getWatches #'painter)
  (doseq [[k w] (.getWatches #'painter)] (remove-watch #'painter k))
  (map str (ImageIO/getWriterFileSuffixes))
  ;; => ("tif" "jpg" "tiff" "bmp" "gif" "png" "wbmp" "jpeg")
  )
