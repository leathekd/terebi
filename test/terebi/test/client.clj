(ns terebi.test.client
  (:use [terebi.client]
        [clojure.test]
        [clojure.java.io :only [file input-stream resource]])
  (:require [clj-http.client :as http]
            [clojure.set]
            [clojure.data.zip.xml :as zf]
            [clojure.string :as str]
            [clojure.xml :as xml]
            [clojure.zip :as zip])
  (:import (java.net URL)))

(def local? true)
(def sid "78874")
(def eid "297992")
(def imdb "tt0303461")
(def zap2it "EP00524463")

(defn config []
  (read-string (slurp (resource "test_config.clj"))))

(defn fixture-file [name]
  (let [file (file (str (.getPath (resource "")) "fixtures/" name))]
    (if (.exists file)
      (read-string (slurp file))
      (throw (Exception. (str "Invalid fixture file: " name))))))

(defmacro maybe-local [response-file & body]
  `(with-redefs [http-post (if local?
                             (constantly (fixture-file ~response-file))
                             http-post)
                 http-get (if local?
                            (constantly (fixture-file ~response-file))
                            http-get)]
     ~@body))

(deftest test-http-get
  (is (thrown? Exception (http-get "http://wat.thetvdb.com")))
  (try
    (http-get "http://www.thetvdb.com")
    (catch Exception e
      (is (not :exception-thrown-fetching-google-dot-com))))
  (with-redefs [http/get (fn [url {headers :headers}]
                           (is ((set (keys headers)) "If-Modified-Since"))
                           {:status 200})]
    (http-get "http://www.google.com" "2012-01-01")))

(deftest test-normalize-keys
  (is (= [:dvd-disc-id "shouldnt-change"]
         (map normalize-keys ["dvd-discid" "shouldnt-change"]))))

(deftest test-make-key
  (is (= :this-is-right (make-key "This_IS_right")))
  (is (= :series-id (make-key "seriesID")))
  (is (= :series-id (make-key "seriesid")))
  (is (= :series-id (make-key "series_id"))))

(deftest test-extract-fields
  (is (= {:one "one" :series-id "123"}
         (-> "<Data><Test><One>one</One><seriesID>123</seriesID></Test></Data>"
             (.getBytes "UTF-8")
             (input-stream)
             (xml/parse)
             (zip/xml-zip)
             (zf/xml1-> :Test)
             (extract-fields)))))

(deftest test-mirrors
  (let [response (maybe-local "00mirrors.xml" (mirrors (config)))]
    (is (<= 1 (count response)))
    (is (every? (partial = [:xml :banners :zips :id :mirror-path :typemask])
                (map keys response)))
    (is ((set response) {:xml true :banners true :zips true :id "1"
                         :mirror-path "http://thetvdb.com" :typemask "7"}))))

(deftest test-languages
  (let [response (maybe-local "01languages.xml" (languages (config)))]
    (is (<= 23 (count response)))
    (is (every? #(= [:name :abbreviation :id] (keys %)) response))
    (is (= {:name "English", :abbreviation "en", :id "7"}
           (first (sort-by (comp #(Integer. %) :id) response))))))

(deftest test-preferred-language
  (is (= {:name "English" :abbreviation "en" :id "7"}
         (maybe-local "02User_PreferredLanguage.php"
                      (preferred-language (config) (:account-id (config)))))))

(deftest test-favorites
  (let [add (maybe-local "03User_Favorites.php"
                         (add-favorite (config) (:account-id (config)) sid))
        lst (maybe-local "04User_Favorites.php"
                         (favorite-series (config) (:account-id (config))))
        rem (maybe-local "05User_Favorites.php"
                         (remove-favorite (config) (:account-id (config)) sid))]
    (is (= #{"78874"}  (clojure.set/difference (set add) (set rem))))
    (is (= add lst))))

(deftest test-ratings
  (let [a-rate (maybe-local "06GetRatingsForUser.php"
                            (ratings (config) (:account-id (config))))
        s-rate (maybe-local "07GetRatingsForUser.php"
                            (ratings (config) (:account-id (config)) sid))
        add-s-rate (maybe-local "08User_Rating.php"
                                (rate-series (config)
                                             (:account-id (config)) sid 10))
        add-e-rate (maybe-local "09User_Rating.php"
                                (rate-episode (config)
                                              (:account-id (config)) eid 10))
        a-rate2 (maybe-local "10GetRatingsForUser.php"
                             (ratings (config) (:account-id (config))))
        s-rate2 (maybe-local "11GetRatingsForUser.php"
                             (ratings (config) (:account-id (config)) sid))
        rem-s-rate (maybe-local "12User_Rating.php"
                                (unrate-series (config)
                                               (:account-id (config)) sid))
        rem-e-rate (maybe-local "13User_Rating.php"
                                (unrate-episode (config)
                                                (:account-id (config)) eid))
        a-rate3 (maybe-local "14GetRatingsForUser.php"
                             (ratings (config) (:account-id (config))))
        s-rate3 (maybe-local "15GetRatingsForUser.php"
                             (ratings (config) (:account-id (config)) sid))]
    (is (empty? a-rate))
    (is (empty? s-rate))
    (is (= [:rating]
           (keys add-s-rate)
           (keys add-e-rate)
           (keys rem-s-rate)
           (keys rem-e-rate)))
    (when local?
      (is (= "9.5" (:rating add-s-rate) (:rating rem-s-rate)))
      (is (= "8.0" (:rating add-e-rate) (:rating rem-e-rate))))
    (is (= [:series] (keys a-rate2)))
    (is (= 1 (count (:series a-rate2))))
    (is (= [:series-id :user-rating :community-rating]
           (keys (first (:series a-rate2)))))
    (is (= sid (:series-id (first (:series a-rate2)))))
    (is (= "10" (:user-rating (first (:series a-rate2)))))
    (is (= #{:series :episode} (set (keys s-rate2))))
    ;; the user-ratings are different.  weird.
    ;; (is (= (first (:series s-rate2)) (first (:series a-rate2))))
    (is (= 1 (count (:episode s-rate2))))
    (is (= [:id :user-rating :community-rating]
           (keys (first (:episode s-rate2)))))
    (is (= eid (:id (first (:episode s-rate2)))))
    (is (= "10" (:user-rating (first (:episode s-rate2)))))
    (is (empty? a-rate3))
    (is (empty? s-rate3))))

(deftest test-series-complete
  (let [all (maybe-local "16en.xml" (series-complete (config) sid))
        since (:last-modified all)
        delta (maybe-local "17en.xml" (series-complete (config) sid since))]
    (is (= "Firefly" (:series-name all)))
    (is (= [:status :banner :network :airs-day-of-week :added-by :overview
            :rating-count :first-aired :airs-time :series-id :language :runtime
            :added :genre :rating :last-updated :actors :series-name
            :last-modified :poster :zap2it-id :id :imdb-id :content-rating
            :seasons :fanart :network-id]
           (keys all)))
    (is (= [:dvd-disc-id :ep-img-flag :combined-season :guest-stars :overview
            :season-id :absolute-number :rating-count :dvd-chapter :director
            :first-aired :combined-episodenumber :episode-number :series-id
            :language :production-code :episode-name :rating :filename
            :last-updated :dvd-episode-number :id :imdb-id :season-number
            :writer :dvd-season]
           (keys (first (get (:seasons all) "1")))))
    (is (nil? delta))
    (is (= 2 (count (:seasons all))))
    (is (= 3 (count (get (:seasons all) "0"))))
    (is (= 14 (count (get (:seasons all) "1"))))))

(deftest test-series
  (let [series (maybe-local "18en.xml" (series (config) sid))
        s-imdb (maybe-local "19GetSeriesByRemoteID.php"
                            (series-imdb (config) imdb))
        s-zap (maybe-local "20GetSeriesByRemoteID.php"
                           (series-zap2it (config) zap2it))]
    (is (= "Firefly" (:series-name series)))
    (is (= s-imdb s-zap))
    (doseq [k (keys s-imdb)
            :when (not (#{:series-id} k))]
      ;; (:series-id series) is 7097?  Weird, not sure what that's all
      ;; about.  Anyway, we can't test for it.
      (is (= (get s-imdb k) (get series k))))))

(deftest test-series-search
  (let [series (maybe-local "18en.xml" (series (config) sid))
        search (maybe-local "21GetSeries.php"
                            (series-search (config) "firefly"))
        hit (first search)]
    (is (= "Firefly" (:series-name hit)))
    (doseq [k (keys hit)
            :when (not (#{:series-id} k))]
      (is (= (get hit k) (get series k))))))

(deftest test-banners
  (let [b (sort-by :id (maybe-local "22banners.xml" (banners (config) sid)))
        b-groups (group-by :banner-type b)]
    (is (= 28 (count b)))
    (is (= ["series" "fanart" "season" "poster"]
           (keys (group-by :banner-type b))))
    ;; series
    (is (= [:id :banner-path :banner-type :banner-type2 :language :rating
            :rating-count]
           (keys (first (get b-groups "series")))))
    (is (= "blank/78874.jpg"
           (:banner-path (first (sort-by :banner-path
                                         (get b-groups "series"))))))
    ;; fanart
    (is (= [:rating-count :thumbnail-path :vignette-path :language :banner-type2
            :banner-type :rating :series-name :id :banner-path :colors]
           (keys (first (get b-groups "fanart")))))
    (is (= "fanart/original/78874-1.jpg"
           (:banner-path (first (sort-by :banner-path
                                         (get b-groups "fanart"))))))
    ;; season
    (is (= [:id :banner-path :banner-type :banner-type2 :language :rating
            :rating-count :season]
           (keys (first (get b-groups "season")))))
    (is (= "seasons/7097-1.jpg"
           (:banner-path (first (sort-by :banner-path
                                         (get b-groups "season"))))))
    ;; poster
    (is (= [:id :banner-path :banner-type :banner-type2 :language :rating
            :rating-count]
           (keys (first (get b-groups "poster")))))
    (is (= "posters/78874-1.jpg"
           (:banner-path (first (sort-by :banner-path
                                         (get b-groups "poster"))))))))

(deftest test-episodes
  (let [by-id (maybe-local "23en.xml"
                           (episode (config) eid))
        by-abs (maybe-local "24en.xml"
                            (episode (config) sid (:absolute-number by-id)))
        by-nums (maybe-local "25en.xml"
                             (episode (config) sid
                                      (:season-number by-id)
                                      (:episode-number by-id)))
        by-nums-def (maybe-local "26en.xml"
                                 (episode (config) sid
                                          (:season-number by-id)
                                          (:episode-number by-id)
                                          :default))
        by-nums-dvd (maybe-local "27en.xml"
                                 (episode (config) sid
                                          (:season-number by-id)
                                          (:episode-number by-id)
                                          :dvd))
        by-nums-abs (maybe-local "28en.xml"
                                 (episode (config) sid
                                          (:season-number by-id)
                                          (:episode-number by-id)
                                          :absolute))
        [_ y m d] (re-find #"(\d\d\d\d)-(\d\d)-(\d\d)" (:first-aired by-id))
        by-date (maybe-local "29GetEpisodeByAirDate.php"
                             (episode-by-aired-date (config) sid y m d))
        common-keys (clojure.set/intersection (set (keys by-date))
                                              (set (keys by-id)))]
    (is (= "Jaynestown"
           (:episode-name by-id)
           (:episode-name by-nums-def)))
    (is (= "2002-10-18" (:first-aired by-id)))
    (is (= by-id by-abs by-nums by-nums-def))
    (is (= "Shindig" (:episode-name by-nums-dvd)))
    (is (= "Serenity" (:episode-name by-nums-abs)))
    (doseq [k common-keys
            ;; more differences  -_-
            :when (not (#{:rating :overview} k))]
      (is (= (get by-id k) (get by-date k))))))

(defn clean-test-data [resp]
  (update-in resp [:trace-redirects 0]
             #(-> %
                  (str/replace (re-pattern (:api-key (config))) "API-KEY-HERE")
                  (str/replace (re-pattern (:account-id (config)))
                               "ACCOUNT-ID-HERE"))))

(defn spit-test-data [url index text]
  (-> url
      (URL.)
      (.getPath)
      (file)
      (.getName)
      ((partial format "./test/fixtures/%02d%s" index))
      (file)
      (spit text)))

(defn gather-test-data [cfg series-id episode-id imdb-id zap2it-id]
  (let [old-http-get http-get
        old-http-post http-post
        count (atom 0)]
    (with-redefs [http-get (fn [url & [since]]
                             (println "Fetching" url)
                             (let [resp (clean-test-data
                                         (old-http-get url since))]
                               (spit-test-data url @count resp)
                               (swap! count inc)
                               resp))
                  http-post (fn [url]
                              (println "Fetching" url)
                              (let [resp (clean-test-data (old-http-post url))]
                                (spit-test-data url @count resp)
                                (swap! count inc)
                                resp))]
      (mirrors cfg)
      (languages cfg)
      (preferred-language cfg (:account-id cfg))
      (add-favorite cfg (:account-id cfg) series-id)
      (favorite-series cfg (:account-id cfg))
      (remove-favorite cfg (:account-id cfg) series-id)
      (ratings cfg (:account-id cfg))
      (ratings cfg (:account-id cfg) series-id)
      (rate-series cfg (:account-id cfg) series-id 10)
      (rate-episode cfg (:account-id cfg) episode-id 10)
      (ratings cfg (:account-id cfg))
      (ratings cfg (:account-id cfg) series-id)
      (unrate-series cfg (:account-id cfg) series-id)
      (unrate-episode cfg (:account-id cfg) episode-id)
      (ratings cfg (:account-id cfg))
      (ratings cfg (:account-id cfg) series-id)
      (let [last-mod (:last-modified (series-complete cfg series-id))]
        (series-complete cfg series-id last-mod))
      (series cfg series-id)
      (series cfg :imdb imdb-id)
      (series cfg :zap2it zap2it-id)
      (series-search cfg "firefly")
      (banners cfg series-id)
      (let [ep (episode cfg episode-id)
            [_ y m d] (re-find #"(\d\d\d\d)-(\d\d)-(\d\d)" (:first-aired ep))]
        (episode cfg series-id (:absolute-number ep))
        (episode cfg series-id (:season-number ep) (:episode-number ep))
        (episode cfg series-id (:season-number ep) (:episode-number ep)
                 :default)
        (episode cfg series-id (:season-number ep) (:episode-number ep) :dvd)
        (episode cfg series-id (:season-number ep) (:episode-number ep)
                 :absolute)
        (episode-by-aired-date cfg series-id y m d))))
  "Done.")
