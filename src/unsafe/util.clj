(ns unsafe.util
  (:require [unsafe.core :as unsafe])
  (:import [java.lang.reflect Field Modifier]))

(defn all-nonstatic-fields
  [^Class c]
  (let [is-static?      (filter #(not (Modifier/isStatic (.getModifiers ^Field %))))
        declared-fields (mapcat #(.getDeclaredFields ^Class %))
        not-nil         (take-while #(some? %))
        xf (comp not-nil declared-fields is-static?)]
     (sequence xf (iterate #(.getSuperclass ^Class %) c))))

(defn ^long sizeof
  "Returns the shallow size of an Object."
  [^Object obj]
  (let [max-size (apply max (map #(unsafe/object-field-offset %)
                                 (all-nonstatic-fields (.getClass obj))))]
    (long (* 8 (+ 1 (/ max-size 8))))))

(defmulti compare-and-swap (fn [obj offset expected x]
                             (mapv class [obj offset expected x])))
(defmethod compare-and-swap
  [Object long int int] [obj offset expected x]
  (unsafe/CAS-int obj offset expected x))
(defmethod compare-and-swap
  [Object long long long] [obj offset expected x]
  (unsafe/CAS-long obj offset expected x))
(defmethod compare-and-swap
  [Object long Object Object] [obj offset expected x]
  (unsafe/CAS-obj obj offset expected x))

;; Getters and Setters
;; {:type int, :volatile? true, :args [obj offset]}
;; (unsafe-get {:type int, :volatile? true, :args [obj offset]})
;; ---
;; normal get: (get map key),   i.e. (get from with)
;; (unsafe/get obj offset int), i.e. (get from with as)
;; (unsafe/get address int),    i.e. (get from as)
(defmulti unsafe-get :type)
(defmulti unsafe-put :type)

(defmethod unsafe-get Boolean
  [{args :args volatile :volatile?}]
  (if volatile
    (apply unsafe/get-boolean-volatile args)
    (apply unsafe/get-boolean args)))
;; (defmethod unsafe-put Boolean
;;   [{args :args volatile :volatile?}]
;;   (if volatile
;;     (apply unsafe/put-boolean-volatile args)
;;     (apply unsafe/put-boolean args)))

(defmethod unsafe-get Byte
  [{args :args volatile :volatile?}]
  (if volatile
    (apply unsafe/get-byte-volatile args)
    (apply unsafe/get-byte args)))
(defmethod unsafe-put Byte
  [{args :args volatile :volatile?}]
  (if volatile
    (apply unsafe/put-byte-volatile args)
    (apply unsafe/put-byte args)))

(defmethod unsafe-get Character
  [{args :args volatile :volatile?}]
  (if volatile
    (apply unsafe/get-char-volatile args)
    (apply unsafe/get-char args)))
(defmethod unsafe-put Character
  [{args :args volatile :volatile?}]
  (if volatile
    (apply unsafe/put-char-volatile args)
    (apply unsafe/put-char args)))

(defmethod unsafe-get Double
  [{args :args volatile :volatile?}]
  (if volatile
    (apply unsafe/get-double-volatile args)
    (apply unsafe/get-double args)))
(defmethod unsafe-put Double
  [{args :args volatile :volatile?}]
  (if volatile
    (apply unsafe/put-double-volatile args)
    (apply unsafe/put-double args)))

(defmethod unsafe-get Float
  [{args :args volatile :volatile?}]
  (if volatile
    (apply unsafe/get-float-volatile args)
    (apply unsafe/get-float args)))
(defmethod unsafe-put Float
  [{args :args volatile :volatile?}]
  (if volatile
    (apply unsafe/put-float-volatile args)
    (apply unsafe/put-float args)))

(defmethod unsafe-get Integer
  [{args :args volatile :volatile?}]
  (if volatile
    (apply unsafe/get-int-volatile args)
    (apply unsafe/get-int args)))
(defmethod unsafe-put Integer
  [{args :args volatile :volatile?}]
  (if volatile
    (apply unsafe/put-int-volatile args)
    (apply unsafe/put-int args)))

(defmethod unsafe-get Long
  [{args :args volatile :volatile?}]
  (if volatile
    (apply unsafe/get-long-volatile args)
    (apply unsafe/get-long args)))
(defmethod unsafe-put Long
  [{args :args volatile :volatile?}]
  (if volatile
    (apply unsafe/put-long-volatile args)
    (apply unsafe/put-long args)))

(defmethod unsafe-get Object
  [{args :args volatile :volatile?}]
  (if volatile
    (apply unsafe/get-obj-volatile args)
    (apply unsafe/get-obj args)))
(defmethod unsafe-put Object
  [{args :args volatile :volatile?}]
  (if volatile
    (apply unsafe/put-obj-volatile args)
    (apply unsafe/put-obj args)))

(defmethod unsafe-get Short
  [{args :args volatile :volatile?}]
  (if volatile
    (apply unsafe/get-short-volatile args)
    (apply unsafe/get-short args)))
(defmethod unsafe-put Short
  [{args :args volatile :volatile?}]
  (if volatile
    (apply unsafe/put-short-volatile args)
    (apply unsafe/put-short args)))
