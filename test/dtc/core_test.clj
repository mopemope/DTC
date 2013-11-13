(ns dtc.core-test
  (:require [clojure.test :refer :all]
            [dtc.core :refer :all]))

(deftest test-string-to-date
  (let [expect (parse-date "2013-11-01")]
    (is (= expect (convert "2013-11-01" java.util.Date)))
    (is (= expect (convert "2013-11-01" :date)))
    (is (= expect (convert "2013/11/01" :date {:format "yyyy/MM/dd"})))))

(deftest test-string-to-sql-timestamp
  (let [expect (java.sql.Timestamp. (.getTime (parse-date "2013-11-01")))]
    (is (= expect (convert "2013-11-01 00:00:00" java.sql.Timestamp)))
    (is (= expect (convert "2013-11-01 00:00:00" :sql-timestamp)))
    (is (= expect (convert "2013/11/01" :sql-timestamp {:format "yyyy/MM/dd"})))))

(deftest test-string-to-sql-date
  (let [expect (java.sql.Date. (.getTime (parse-date "2013-11-01")))]
    (is (= expect (convert "2013-11-01" java.sql.Date)))
    (is (= expect (convert "2013-11-01" :sql-date)))
    (is (= expect (convert "2013/11/01" :sql-date {:format "yyyy/MM/dd"})))))

(deftest test-string-to-int
  (let [expect (Integer. 10000)]
    (is (= expect (convert "10000" Integer)))
    (is (= expect (convert "10000" :int)))))

(deftest test-string-to-long
  (let [expect (Long. 10000)]
    (is (= expect (convert "10000" Long)))
    (is (= expect (convert "10000" :long)))))

(deftest test-string-to-float
  (let [expect (Float. 12.34)]
    (is (= expect (convert "12.34" Float)))
    (is (= expect (convert "12.34" :float)))))

(deftest test-string-to-double
  (let [expect (Double. 12.34)]
    (is (= expect (convert "12.34" Double)))
    (is (= expect (convert "12.34" :double)))))

(deftest test-string-to-decimal
  (let [expect (BigDecimal. 1000000)]
    (is (= expect (convert "1000000" BigDecimal)))
    (is (= expect (convert "1000000" :decimal)))))

(deftest test-string-to-number
  (is (= 0.42M (convert "0.42" Number)))
  (is (= 0.42M (convert "42e-2" Number)))
  (is (= 1M (convert "1" Number)))
  (is (= 1M (convert "1" :number))))

(deftest test-string-to-boolean
  (is (= false (convert "false" Boolean)))
  (is (= false (convert "nope" Boolean)))
  (is (= true (convert "true" Boolean)))
  (is (= true (convert "true" :boolean))))

(deftest test-date-to-string
  (let [dt (parse-date "2013-11-01")]
    (is (= "2013-11-01" (convert dt :string)))
    (is (= "11/01/13"(convert dt :string {:format "M/dd/yy"})))
    (is (= "11/01/13"(convert dt :string {:format "M/dd/yy"})))))

(deftest test-date-to-long
  (let [dt (parse-date "2013-11-03")]
    (is (= (.getTime dt) (convert dt Long)))
    (is (= (.getTime dt) (convert dt :long)))))

(deftest test-long-to-date
  (let [dt (parse-date "2013-11-01")]
    (is (= dt (convert (.getTime dt) java.util.Date)))
    (is (= dt (convert (.getTime dt) :date)))))

(deftest test-integer-to-string
  (is (= "1000" (convert 1000 String)))
  (is (= "1000" (convert 1000 :string))))

(deftest test-double-to-string
  (is (= "0.3" (convert (double 0.3) String)))
  (is (= "0.3" (convert (double 0.3) :string))))

(deftest test-float-to-string
  (is (= "0.3" (convert (float 0.3) String)))
  (is (= "0.3" (convert (float 0.3) :string))))

(deftest test-big-decimal-to-string
  (is (= "0.3" (convert (BigDecimal. "0.3") String)))
  (is (= "0.3" (convert (BigDecimal. "0.3") :string))))

(deftest test-convert-nil
  (is (nil? (convert nil Number)))
  (is (nil? (convert nil String))))

(deftest test-format-number
  (is (= "5,000.42" (convert 5000.42 String "%,.2f"))) 
  (is (= "5,000" (convert 5000 String "%,d"))))

(deftest test-convert-map
  (let [dt (parse-date "2013-11-01")
        fmt {:d1 java.util.Date
             :d2 [:string {:format "M/dd/yy"}]}
        in {:d1 "2013-11-01"
            :d2 dt
            :d3 "Something else"}]
    (is (=
          {:d1 dt :d2 "11/01/13" :d3 "Something else"}
          (convert-map in fmt)))))




