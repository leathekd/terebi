(ns terebi.client
  (:use [clojure.java.io :only [input-stream]]
        [slingshot.slingshot :only [throw+]])
  (:require [clj-http.client :as http]
            [clojure.data.zip.xml :as zf]
            [clojure.string :as str]
            [clojure.xml :as xml]
            [clojure.zip :as zip]))

(defn handle-response [response]
  (condp = (:status response)
    200 response
    304 response
    (throw+ response)))

(defn http-get
  "Utility function to make HTTP GETs, optionally with an
  if-modified-since header"
  [url & [since]]
  (handle-response (http/get url
                             {:headers (if since {"If-Modified-Since" since} {})
                              :throw-exceptions false})))

(defn http-post
  "Utility function to make HTTP POSTs"
  [url]
  (handle-response (http/post url {:throw-exceptions false})))

(defn body-xml
  "Extract and parse the body of a get response"
  [response]
  (when (:body response)
    (xml/parse (input-stream (.getBytes (:body response) "UTF-8")))))

(defn normalize-keys
  "TheTVDB XML has a few elements that aren't formatted as you'd expect in
  Clojure.  Normalize-keys attempts to address this."
  [k]
  (condp = (name k)
    "dvd-discid" :dvd-disc-id
    "dvd-episodenumber" :dvd-episode-number
    "lastupdated" :last-updated
    "mirrorpath" :mirror-path
    "mirrorupdate" :mirror-update
    "seasonid" :season-id
    "seriesid" :series-id
    k))

(defn hyphenize
  "Transform spaces, underscores, and capitalization into hyphens"
  [s]
  (-> s
      (str/replace #"_|\s+" "-")
      (str/replace #"^([A-Z]{2,})" #(str (.toLowerCase (nth % 1)) "-"))
      (str/replace #"(.)([A-Z]+)" #(str (nth % 1) "-" (nth % 2)))
      (.toLowerCase)
      (str/replace #"-+" "-")))

(defn make-key
  "Take the XML element name and turn it into a sensibly named key"
  [k]
  (-> k
      name
      hyphenize
      normalize-keys
      keyword))

(def multi-field?
  "Fields inside the response that could have multiple values, separated by |"
  #{:genre :actors :guest-stars :director :writer})

(defn extract-fields
  "Given an xml element, extract all child elements into a hash map.  Does not
  handle nested elements or elements with only a string."
  [element]
  (when (seq element)
    (let [fields (map :tag (zip/children element))]
      (into {}
            (for [field fields
                  :let [k (make-key field)
                        v (zf/xml1-> element field zf/text)]]
              [k (if (multi-field? k)
                   (filter seq (clojure.string/split v #"\|"))
                   v)])))))

(defn url
  "Returns the base url for a request based on the endpoint and config."
  [{:keys [api-key mirror-path]} endpoint]
  (if (re-find #"\.php" endpoint)
    (format (str "%s/api/%s") mirror-path endpoint)
    (format (str "%s/api/%s/%s") mirror-path api-key endpoint)))

(defn call-api
  "Call the TVDB API.  Passed parameters, if any will be turned into the
  query string."
  ([config endpoint elt-name]
     (call-api config endpoint nil elt-name))
  ([config endpoint params elt-name & [call-type]]
     (let [base-url (url config endpoint)
           query (if params
                   (str "?" (http/generate-query-string params))
                   "")]
       (when-let [resp (body-xml (if (= :write call-type)
                                   (http-post (str base-url query))
                                   (http-get (str base-url query))))]
         (map extract-fields (-> resp
                                 (zip/xml-zip)
                                 (zf/xml-> elt-name)))))))

(defn mirrors
  "Fetch the list of mirrors.  This is the only call that does not require a
  :mirror-path as part of the config."
  [config]
  (let [mirrors (call-api (assoc config :mirror-path "http://thetvdb.com")
                          "mirrors.xml" :Mirror)]
    (map (fn [mirror]
           ;; TODO - gotta be a better way
           (condp = (:typemask mirror)
             "7" (assoc mirror :zips true :banners true :xml true)
             "6" (assoc mirror :zips true :banners true)
             "5" (assoc mirror :zips true :xml true)
             "4" (assoc mirror :zips true)
             "3" (assoc mirror :banners true :xml true)
             "2" (assoc mirror :banners true)
             "1" (assoc mirror :xml true)))
         mirrors)))

(defn languages
  "Get the list of supported languages."
  [{:keys [mirror-path api-key] :as config}]
  (call-api config "languages.xml" :Language))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn preferred-language
  "Get the user's preferred language."
  [config account-id]
  (first (call-api config
                   "User_PreferredLanguage.php"
                   {:accountid account-id}
                   :Language)))

(defn favorite-series
  "Get a list of the series ids for the user's favorite series."
  [config account-id]
  (let [response (->> account-id
                      (format "User_Favorites.php?accountid=%s")
                      (url config)
                      (http-get)
                      (body-xml)
                      (zip/xml-zip))]
    (zf/xml-> response :Series zf/text)))

(defn- change-favorites
  "Internal function that actually makes the API call to add or remove a series
  from the list of favorites."
  [{:keys [mirror-path]} account-id type series-id]
  (let [response
        (body-xml (http-post
                   (format (str "%s/api/User_Favorites.php?accountid=%s"
                                "&type=%s&seriesid=%s")
                           mirror-path account-id type series-id)))]
    (zf/xml-> (zip/xml-zip response) :Series zf/text)))

(defn add-favorite
  "Add the given series-id to the account's list of favorites."
  [config account-id series-id]
  (change-favorites config account-id "add" series-id))

(defn remove-favorite
  "Remove the given series-id from the account's list of favorites."
  [config account-id series-id]
  (change-favorites config account-id "remove" series-id))

(defn ratings
  "Fetch all the series that a user has rated.  If a series-id is passed in,
  return the rating for that series and any episodes that the user has rated."
  [{:keys [api-key mirror-path]} account-id & [series-id]]
  (let [query-params {:apikey api-key
                      :accountid account-id}
        query-string (http/generate-query-string
                      (if series-id
                        (assoc query-params :seriesid series-id)
                        query-params))
        xml-doc (zip/xml-zip
                 (body-xml (http-get (format "%s/api/GetRatingsForUser.php?%s"
                                             mirror-path
                                             query-string))))]
    ;; extract both series and episodes from the request and assoc
    ;; them to the response map only if there is something to assoc.
    (reduce
     (fn [m k] (let [v (filter seq (map extract-fields (zf/xml-> xml-doc k)))]
                 (if-not (empty? v)
                   (assoc m (keyword (.toLowerCase (name k))) v)
                   m)))
     {} [:Series :Episode])))

(defn- rate
  "Internal function that actually sends the rating API request."
  [config account-id elt-name type id rating]
  (first (call-api config
                   "User_Rating.php"
                   {:accountid account-id
                    :itemtype (name type)
                    :itemid id
                    :rating rating}
                   elt-name
                   :write)))

(defn rate-series
  "Add a rating to a series.  Valid ratings are between 0-10.  A rating of 0
  will unrate the series."
  [config account-id series-id rating]
  (rate config account-id :Series :series series-id (-> rating
                                                        (Integer.)
                                                        (max 0)
                                                        (min 10))))

(defn unrate-series
  "Convenience function to unrate the series."
  [config account-id series-id]
  (rate-series config account-id series-id 0))

(defn rate-episode
  "Add a rating to a episode.  Valid ratings are between 0-10.  A rating of 0
  will unrate the episode."
  [config account-id episode-id rating]
  (rate config account-id :Episode :episode episode-id (-> rating
                                                           (Integer.)
                                                           (max 0)
                                                           (min 10))))

(defn unrate-episode
  "Convenience function to unrate the episode."
  [config account-id episode-id]
  (rate-episode config account-id episode-id 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Series
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn series-complete
  "Fetch a series and all of its episodes.  In addition to the series
  information, a :last-modified key is added and can be sent into
  subsequent calls in order to get just the new items since that date."
  [{:keys [language] :as config} series-id & [since]]
  (let [url (url config (format "series/%s/all/%s.xml"
                                series-id (or language "en")))
        response (http-get url since)
        last-modified (get-in response [:headers "last-modified"])]
    (when-let [resp-xml (body-xml response)]
      (let [xml (zip/xml-zip resp-xml)
            series (extract-fields (zf/xml1-> xml :Series))
            seasons (group-by :season-number
                              (map extract-fields
                                   (zf/xml-> xml :Episode)))]
        (assoc series :seasons seasons :last-modified last-modified)))))

(defn series
  "Fetch just the series information with no episodes.  Takes either the TVDB ID
  or can take an IMDB or Zap2it ID.  When using the IMDB or Zap2it ID the
  keyword :imdb or :zap2it should be passed in as remote-service."
  ([config series-id]
     (first (call-api config
                      (format "series/%s/%s.xml" series-id
                              (or (:language config) "en"))
                      :Series)))
  ([config remote-service remote-id]
     (first (call-api config
                      "GetSeriesByRemoteID.php"
                      {(if (= remote-service :imdb) :imdbid :zap2it) remote-id
                       :language (or (:language config) "en")}
                      :Series))))

(defn series-imdb
  "Convenience function for fetching a series by IMDB ID."
  [config id]
  (series config :imdb id))

(defn series-zap2it
  "Convenience function for fetching a series by Zap2it ID."
  [config id]
  (series config :zap2it id))

(defn series-search
  "Fetch basic series data (no episodes) based on a search term."
  [{:keys [api-key language] :as config} series-name]
  (call-api config
            "GetSeries.php"
            {:apikey api-key
             :seriesname series-name
             :language (or language "en")}
            :Series))

(defn banners
  "Fetch the banner information (series/season images, posters, fanart)
  for a series."
  [config series-id]
  (call-api config
            (format "series/%s/banners.xml" series-id)
            :Banner))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Episode related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn episode
  "Fetch episode information.  Options supported: by ID, by season number plus
  episode number, by absolute number.  Additionally, when providing season
  and episode numbers, an optional sort-type may be passed.  Sort-type is one of
  the following: :dvd, :absolute, or :default (which you don't need to pass in,
  but is supported)."
  ([{:keys [language] :as config} episode-id]
     (first (call-api config
                      (format "episodes/%s/%s.xml"
                              episode-id (or language "en"))
                      :Episode)))
  ([{:keys [language] :as config} series-id absolute-num]
     (first (call-api config
                      (format "series/%s/absolute/%s/%s.xml"
                              series-id absolute-num
                              (or language "en"))
                      :Episode)))
  ([{:keys [language] :as config} series-id season-num episode-num
    & [sort-type]]
     (first (call-api config
                      (format "series/%s/%s/%s/%s/%s.xml"
                              series-id
                              (name (or sort-type :default))
                              season-num episode-num (or language "en"))
                      :Episode))))

(defn episode-by-aired-date
  "Fetch episode information for a series based on when it aired."
  [{:keys [api-key language] :as config} series-id year month day]
  (first (call-api config
                   "GetEpisodeByAirDate.php"
                   {:apikey api-key
                    :seriesid series-id
                    :airdate (format "%s/%s/%s" year month day)
                    :language (or language "en")}
                   :Episode)))
