(defn- inst-in-int [begin end instmap]
  (->> instmap
       (filter #(within? (interval (parse-time begin)
                                   (parse-time end))
                         (parse-time (key %))))
       flatten
       (apply assoc inst-map)))

(defn val-prop-time [variable validator proportion rel-time]
  "True when variable passes validator for >= proportion of instances
    over interval rel-time. Rel-time may be negative.
    Examples:
    'Exercised any amount for 6/7 days in the prior week'
    (val-prop-time \"exercise\" (num-interval 1 nil) 6/7 (weeks -1))

    'Mood was never 1 or 2 in prior month'
    (val-prop-time \"mood\" #{1 2} 0 (months -1))

    'Used caffeine the next day'
    (val-prop-time \"caffeine\" #{0} 0 (days 1))"
  (fn [t]
    (let [times (assoc inst-map t :_,
                  (unparse-time (plus (parse-time t) rel-time)) :_)
          end   (ffirst times)
          begin (first (last times))]
      (->> (:instances ((str->key variable) @active-expt))
           (inst-in-int begin end)
           vals
           (group-by validator)
           (group-by #(or (nil? (key %))
                          (false? (key %))))
           vals
           (map (comp count flatten #(map second %)))
           sort
           (apply /)
           (< proportion)))))
