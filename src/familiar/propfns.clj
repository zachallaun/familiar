(defn val-prop-time [variable value proportion rel-time]
  ; rel-time may be + or -
  (fn [t]
    (let [times (assoc inst-map t :_,
                  (unparse-date (plus (parse-date t) rel-time)) :_)
          end   (ffirst times)
          begin (first (last times))]
      (->> (:instances ((str->key variable) @experiment))
           (filter #(within? (interval (parse-date begin)
                                       (parse-date end)) %))
           ; ????
         ))))
