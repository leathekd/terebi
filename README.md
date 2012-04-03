# terebi

TheTVDB.com is a great community driven database of television shows and episodes.  Terebi is a Clojure client to TheTVDB's API.

Before you can use either the API or Terebi, you must register at http://www.thetvdb.com and then get an API key at http://thetvdb.com/?tab=apiregister.  

Your API key will be at http://thetvdb.com/?tab=userinfo when you're logged in.  Your account ID will be on the same page and is used for several of the API calls.

TheTVDB's API is documented here: http://www.thetvdb.com/wiki/index.php/Programmers_API

## Usage

Most calls require a config map with the following:

```clj
{:api-key "your api key"
 :language "en" ;; optional
 :mirror-path "http://mirror-url"}
```

A brief example.  You'll need to add some series to your list of favorites before this does anything intersting.

```clj
(let [account-id "" ;; fill me in
      cfg {:api-key ""} ;; me too
      cfg (assoc cfg :mirror-path (:mirror-path (first (mirrors cfg))))]
  (println "My favorite series:")
  (doseq [favorite-id (favorite-series cfg account-id)]
    (println (:series-name (series cfg favorite-id)))))
```

The tests have a lot of sample code, as well.

## License

Copyright (C) 2012 David Leatherman

Distributed under the Eclipse Public License, the same as Clojure.
