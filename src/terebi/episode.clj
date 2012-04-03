(ns terebi.episode

  )

(defn episode-dispatch [& args]
  (condp = (count args)
    2 :episode-id
    4 :episode-num
    5 :episode-num
    3 (if (integer? (nth args 2))
        :absolute
        :airdate)))

(defmulti episode "" episode-dispatch)

(defmethod episode :episode-id
  [{:keys [api-key language]} episode-id]
  (-> (format "%s/api/%s/episodes/%s/%s.xml"
              base-url api-key episode-id (or language "en"))
      (fetch-xml)
      (zip/xml-zip)
      (zf/xml1-> :Episode)
      (extract-fields)))

(defmethod episode :episode-num
  [{:keys [api-key language]} series-id season-num episode-num & [sort-type]]
  (-> (format "%s/api/%s/series/%s/%s/%s/%s/%s.xml"
              base-url api-key series-id (name (or sort-type :default))
              season-num episode-num (or language "en"))
      (fetch-xml)
      (zip/xml-zip)
      (zf/xml1-> :Episode)
      (extract-fields)))

(defmethod episode :absolute
  [{:keys [api-key language]} series-id absolute-num]
  (-> (format "%s/api/%s/series/%s/absolute/%s/%s.xml"
              base-url api-key series-id absolute-num (or language "en"))
      (fetch-xml)
      (zip/xml-zip)
      (zf/xml1-> :Episode)
      (extract-fields)))

(defmethod episode :airdate
  [{:keys [api-key language]} series-id airdate]
  (let [query-string (http/generate-query-string
                      {:apikey api-key :seriesid series-id
                       :airdate airdate :language (or language "en")})]
    (-> (format "%s/api/GetEpisodeByAirDate.php?%s"
                base-url query-string)
        (fetch-xml)
        (zip/xml-zip)
        (zf/xml1-> :Episode)
        (extract-fields))))