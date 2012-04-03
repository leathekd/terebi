(ns terebi.test.remote
  (:use [terebi.test.client]
        [clojure.test]))

(deftest ^{:remote true} test-live
  (println "\nTesting remote integration")
  (let [cfg (terebi.test.client/config)]
    (if (or (= #"ACCOUNT-ID-HERE" (:account-id cfg))
            (= #"API-KEY-HERE" (:api-key cfg)))
      (println "You must fill in the account-id and api-key in"
               "terebi/test/test_config.clj before you can run the"
               "remote tests.")
      (with-redefs [terebi.test.client/local? false]
        (doseq [[_ v] (ns-publics 'terebi.test.client)
                :when (and (:test (meta v))
                           (not (:remote (meta v))))]
          ((:test (meta v))))))))
