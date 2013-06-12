(defmacro val-prop-time [variable value proportion rel-time]
  "True if (proportion) of instances of (variable) between t and (rel-time) 
  have (value)."
  `(val-prop-time- ~variable ~value ~proportion (nser ~rel-time "jtime")))

(defn val-prop-time- [variable value proportion rel-time]
  ; rel-time may be + or -
  (fn [t]
    (let [times (assoc inst-map t :_,
                  (unparse-date (plus (parse-date t) rel-time)) :_)
          begin (ffirst times)
          end   (first (last times))]
      (->> (:instances ((keyword variable) @experiment))
           (filter #(within? (interval (parse-date begin)
                                                   (parse-date end))))
           
           ))))
