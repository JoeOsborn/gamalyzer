(ns gamalyzer.ui.colors
    (:require [strokes :refer [d3]]))

(def colors
  (map #(apply d3.rgb %)
       (partition 3 [255 0 128
                     206 93 169
                     167 22 121
                     242 169 224
                     213 43 255
                     41 17 47
                     88 7 128
                     85 54 104
                     117 119 220
                     6 13 69
                     13 33 133
                     16 60 255
                     24 33 52
                     18 141 254
                     105 145 175
                     35 83 115
                     63 137 178
                     76 182 231
                     18 187 255
                     183 228 236
                     16 237 254
                     112 231 241
                     20 88 92
                     30 176 158
                     0 146 103
                     8 214 109
                     97 232 164
                     168 236 182
                     17 68 25
                     18 33 18
                     11 166 6
                     50 106 37
                     96 248 17
                     97 160 30
                     171 222 98
                     182 209 35
                     95 97 20
                     255 255 35
                     161 153 21
                     238 229 87
                     255 223 23
                     163 153 96
                     244 229 160
                     77 70 44
                     47 42 33
                     255 158 19
                     234 163 95
                     157 90 32
                     236 92 24
                     36 15 9
                     226 166 155
                     119 46 36
                     226 101 83
                     169 94 86
                     74 17 12
                     228 32 21
                     165 26 20
                     ; 255 255 255
                     193 193 193
                     149 149 149
                     98 98 98
                     54 54 54
                     29 29 29])))

(def val-colors {})
(def cur-color 0)
(defn wrap [n l]
  (cond
   (>= n l) (wrap (- n l) l)
   (< n 0) (wrap (+ n l) l)
   true n))

(defn pick-color
  ([values]
   (when-not (get val-colors values)
     (set! cur-color (wrap (inc cur-color) (count colors)))
     (set! val-colors (assoc val-colors values cur-color)))
   (.toString (nth colors (get val-colors values))))
  ([values sat]
   (pick-color values)
   (let [col (.hsl (nth colors (get val-colors values)))
         h (.-h col)
         s (.-s col)]
     (.toString (d3.hsl h s sat)))))
