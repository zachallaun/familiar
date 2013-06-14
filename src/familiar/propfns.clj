(defn val-prop-time [variable validator proportion rel-time]
  "True when variable passes validator for >= proportion of instances
  over interval rel-time. Rel-time may be negative.
  Examples:
  'Exercised at least a little 6/7 days in the prior week'
  (val-prop-time \"exercise\" (interval 1 nil) 6/7 (weeks -1))

  'Mood was never 1 or 2 in prior month'
  (val-prop-time \"mood\" #{1 2} 0 (months -1))

  'Used caffeine the next day'
  (val-prop-time \"caffeine\" #{0} 0 (days 1))"
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
