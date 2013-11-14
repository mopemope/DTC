(ns dtc.core
  (:require
   [clojure.string :as string])
  (:import
   [java.util TimeZone]
   [java.text SimpleDateFormat ParseException]))

(def ^:dynamic *date-fmt* "yyyy-MM-dd")
(def ^:dynamic *timestamp-fmt* "yyyy-MM-dd HH:mm:ss")
(def ^:dynamic *time-zone* (TimeZone/getDefault))

(def ^:dynamic *alias* {:string String
                        :boolean Boolean
                        :int Integer
                        :long Long
                        :float Float
                        :double Double
                        :decimal BigDecimal
                        :number BigDecimal
                        :date java.util.Date
                        :sql-date java.sql.Date
                        :sql-timestamp java.sql.Timestamp})

(defprotocol Converter
  (convert
    [from to]
    [from to option]))

(defn- extpand-converters [x p]
  (keep-indexed #(if (odd? %1)
                   `(~%2 ~p)
                   %2) x))

(defn- extpand-converters-options [colls p options]
  (keep-indexed (fn [x y]
                  (if (odd? x)
                    `(~y ~p ~options)
                    y)) colls))

(defmacro defconverter [type-name & body]
  (let [from (gensym)
        to (gensym)
        option (gensym)]
    `(extend-protocol Converter
            ~type-name
            (~'convert
              ([~from ~to]
                 (if (and ~to)
                   (condp = (if (class? ~to) ~to (~to *alias*))
                     ~@(extpand-converters body from)
                     (if (fn? ~to)
                       (~to ~from)
                       (throw (IllegalArgumentException. "No Implementation of Converter"))))
                   (throw (IllegalArgumentException. "To type is nil or false"))))

              ([~from ~to ~option]
                 (if (and ~to)
                   (condp = (if (class? ~to) ~to (~to *alias*))
                     ~@(extpand-converters-options body from option)
                     (if (fn? ~to)
                       (~to ~from ~option)
                       (throw (IllegalArgumentException. "No Implementation of Converter"))))
                   (throw (IllegalArgumentException. "To type is nil or false"))))))))

(defn ^java.util.Date parse-date
  ([^String value] (parse-date value {}))
  ([^String value {:keys [format timezone]
                   :or {format *date-fmt*
                        timezone *time-zone*}}]
     (let [timezone (if (string? timezone) (TimeZone/getTimeZone timezone) timezone)]
       (if (sequential? format)
         (if-let [res (first (filter identity
                                     (map
                                      #(try (parse-date value {:format % :timezone timezone})
                                            (catch ParseException e))
                                      format)))]
           res
           (throw (ParseException. (str "Unparseable date: " value) 0)))
         (let [fmt (SimpleDateFormat. format)]
           (.setTimeZone fmt timezone)
           (.parse fmt value))))))

(defn ^String format-date
  ([value]
     (format-date value {}))
  ([value {:keys [format timezone]
            :or {format *date-fmt*
                 timezone *time-zone*}}]
     (let [fmt (SimpleDateFormat. format)]
       (.setTimeZone fmt timezone)
       (.format fmt value))))

(defn- format-number
  ([x] (str x))
  ([x fmt]
     (format fmt x )))

;; String conveter

(defmacro extend-string-converter [& body]
  `(defconverter String
     ~@body
     String str
     Boolean #(Boolean/parseBoolean %)
     Integer #(Integer/parseInt %)
     Long #(Long/parseLong %)
     Float #(Float/parseFloat %)
     Double #(Double/parseDouble %)
     BigDecimal #(BigDecimal. ^String %)
     Number #(BigDecimal. ^String %)
     java.util.Date parse-date
     java.sql.Date (fn
                     ([x#]
                        (-> (parse-date x#)
                            (.getTime)
                            (java.sql.Date.)))
                     ([x# o#]
                        (-> (parse-date x# o#)
                            (.getTime)
                            (java.sql.Date.))))
     java.sql.Timestamp (fn
                          ([x#]
                             (-> (parse-date x#)
                                 (.getTime)
                                 (java.sql.Timestamp.)))
                          ([x# o#]
                             (-> (parse-date x# o#)
                                 (.getTime)
                                 (java.sql.Timestamp.))))))


;; Integer converter

(defmacro extend-integer-converter [& body]
  `(defconverter Integer
     ~@body
     String format-number
     Long #(.longValue ^Integer %)
     Float #(Float/parseFloat (str %))
     Double #(Double/parseDouble (str %))
     BigDecimal #(BigDecimal. ^Integer %)
     Number #(BigDecimal. ^Integer %)))

;; Long converter

(defmacro extend-long-converter [& body]
  `(defconverter Long
     ~@body
     String format-number
     Integer #(.intValue ^Long %)
     Float #(Float/parseFloat (str %))
     Double #(Double/parseDouble (str %))
     BigDecimal #(BigDecimal/valueOf ^Long %)
     Number #(BigDecimal/valueOf ^Long %)
     java.util.Date #(java.util.Date. ^Long %)
     java.sql.Date #(java.sql.Date. ^Long %)
     java.sql.Timestamp #(java.sql.Timestamp. %)))

(defmacro extend-float-converter [& body]
  `(defconverter Float
     ~@body
     String format-number
     Integer #(.intValue ^Float %)
     Float #(.floatValue ^Float %)
     BigDecimal #(BigDecimal. ^String (.toString ^Float %))
     Number #(BigDecimal. ^String (.toString ^Float %))))

(defmacro extend-double-converter [& body]
  `(defconverter Double
     ~@body
     String format-number
     Integer #(.intValue ^Double %)
     Float #(.floatValue ^Double %)
     BigDecimal #(BigDecimal. ^Double %)
     Number #(BigDecimal. ^Double %)))

;; BigDecimal converter

(defmacro extend-decimal-converter [& body]
  `(defconverter BigDecimal
     ~@body
     String format-number
     Integer #(.intValue ^BigDecimal %)
     Long #(.longValue ^BigDecimal %)
     Float #(.floatValue ^BigDecimal %)
     Double #(.doubleValue ^BigDecimal %)
     java.util.Date #(java.util.Date. (.longValue ^BigDecimal %))
     java.sql.Date #(java.sql.Date. (.longValue ^BigDecimal %))
     java.sql.Timestamp #(java.sql.Timestamp. (.longValue ^BigDecimal %))))


(defmacro extend-date-converter [& body]
  `(defconverter java.util.Date
     ~@body
     String format-date
     Integer #(convert (.getTime ^java.util.Date %) :int)
     Long #(.getTime ^java.util.Date %)
     Float #(Float/parseFloat (str (.getTime ^java.util.Date %)))
     Double #(Double/parseDouble (str (.getTime ^java.util.Date %)))
     java.sql.Date #(java.sql.Date. (.getTime ^java.util.Date %))
     java.sql.Timestamp #(java.sql.Timestamp. (.getTime ^java.util.Date %))))

(defmacro extend-sql-date-converter [& body]
  `(defconverter java.sql.Date
     ~@body
     String format-date
     Integer #(convert (.getTime ^java.sql.Date %) :int)
     Long #(.getTime ^java.sql.Date %)
     Float #(Float/parseFloat (str (.getTime ^java.sql.Date %)))
     Double #(Double/parseDouble (str (.getTime ^java.sql.Date %)))
     java.util.Date #(java.util.Date. (.getTime ^java.sql.Date %))
     java.sql.Timestamp #(java.sql.Timestamp. (.getTime ^java.sql.Date %))))

(defmacro extend-sql-time-converter [& body]
  `(defconverter java.sql.Timestamp
     ~@body
     String format-date
     Integer #(convert (.getTime ^java.sql.Timestamp %) :int)
     Long #(.getTime ^java.sql.Timestamp %)
     Float #(Float/parseFloat (str (.getTime ^java.sql.Timestamp %)))
     Double #(Double/parseDouble (str (.getTime ^java.sql.Timestamp %)))
     java.util.Date #(java.util.Date. (.getTime ^java.sql.Timestamp %))
     java.sql.Date #(java.sql.Date. (.getTime ^java.sql.Timestamp %))))

(defmacro extend-nil-converter [& body]
  `(defconverter nil
     ~@body
     (throw (IllegalArgumentException. "Value is nil or No Implementation of Converter"))))

(extend-string-converter)
(extend-integer-converter)
(extend-long-converter)
(extend-float-converter)
(extend-double-converter)
(extend-decimal-converter)
(extend-date-converter)
(extend-sql-date-converter)
(extend-sql-time-converter)
(extend-nil-converter)

(defn not-empty? [v]
  (and v
       (= String (type v))
       (not= "" (string/trim v))))

(defn convert-map
  [in params & {:keys [ignore-empty] :or {ignore-empty false}}]
  (into {}
        (for [[k v] in :let [p (k params)]]
          (if (and p (if ignore-empty (not-empty? v) true))
            (if (vector? p)
              [k (convert v (first p) (second p))]
              [k (convert v p)])
            [k v]))))

(defn replace-keys [m match replace]
  (into {}
        (map (fn [[x y]]
               (vector
                (keyword (string/replace (name x) match replace)) y))
             m)))

(defn default-value [in conversion-params]
  (into {}
        (for [[k v] in :let [default (k conversion-params)]]
          (if (and default (not (not-empty? v)))
            [k default]
            [k v]))))

(defn remove-empty [m]
  (into {}
        (filter (fn [[x y]]
                  (not-empty? y)) m)))

(defn ->insert-key [m]
  (replace-keys m "-" "_"))

(defn ->field-key [m]
  (replace-keys m "_" "-"))

