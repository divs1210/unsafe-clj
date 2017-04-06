(ns unsafe.core
  "A wrapper lib for sun.misc.Unsafe"
  {:author "Ryan Serazin"}
  (:import clojure.reflect.Field
           sun.misc.Unsafe))

(set! *warn-on-reflection* true)

(defn- ^Unsafe make-unsafe []
  "reflection!"
  (let [constructor (.getDeclaredConstructor Unsafe nil)]
    (.setAccessible constructor true)
    (.newInstance constructor nil)))

;todo secure this
(defonce ^Unsafe the-unsafe
  (make-unsafe))

(defn ^long address-size
  []
  (.addressSize the-unsafe))

(defn ^Object allocate-instance
  [^java.lang.Class c]
  (.allocateInstance the-unsafe c))

(defn ^long allocate-memory
  [^long b]
  (.allocateMemory the-unsafe b))

(defn ^int array-base-offset
  [^java.lang.Class c]
  (.arrayBaseOffset the-unsafe c))

(defn ^int array-index-scale
  [^java.lang.Class c]
  (.arrayIndexScale the-unsafe c))

(defn ^boolean CAS-int
  [^Object obj ^long offset ^long expected ^long x]
  (let [expected (int expected)
        x        (int x)]
    (.compareAndSwapInt the-unsafe obj offset expected x)))

(defn ^boolean CAS-long
  [^Object obj ^long offset ^long expected ^long x]
  (.compareAndSwapLong the-unsafe obj offset expected x))

(defn ^boolean CAS-obj
  [^Object obj ^long offset ^long expected ^long x]
  (.compareAndSwapObject the-unsafe obj offset expected x))

(defn copy-memory
  [from foffset to toffset b]
  (let [foffset (long foffset)
        toffset (long toffset)
        b       (long b)]
  (.copyMemory the-unsafe
               from foffset to toffset b)))

(defn define-anonymous-class
  [^Class host ^bytes data cp-patches]
  (.defineAnonymousClass the-unsafe
                         host
                         data
                         (object-array cp-patches)))

(defn define-class
  [^String the-name b off len & util]
  (let [b   (bytes b)
        off (int off)
        len (int len)
        [loader domain] util]
    (if (and (nil? loader) (nil? domain))
      (.defineClass the-unsafe the-name b off len)
      (.defineClass the-unsafe the-name b off len loader domain))))

(defn ensure-class-init
  [^Class c]
  (.ensureClassInitialized the-unsafe c))

(defn free-memory
  [^long addr]
  (.freeMemory the-unsafe addr))

(defn ^long get-address
  [^long addr]
  (.getAddress the-unsafe addr))

(defn ^boolean get-boolean
  [^Object obj ^long offset]
  (.getBoolean the-unsafe obj offset))

(defn ^boolean get-boolean-volatile
  [^Object obj ^long offset]
  (.getBooleanVolatile the-unsafe obj offset))

(defn ^byte get-byte
  ([^long addr]
   (.getByte the-unsafe addr))
  ([^Object obj ^long offset]
    (.getByte the-unsafe obj offset)))

(defn ^byte get-byte-volatile
  [^Object obj ^long offset]
  (.getByteVolatile the-unsafe obj offset))

(defn ^char get-char
  ([^long addr]
   (.getChar the-unsafe addr))
  ([^Object obj ^long offset]
   (.getChar the-unsafe obj offset)))

(defn ^char get-char-volatile
  [^Object obj offset]
  (let [offset (long offset)]
    (.getCharVolatile the-unsafe obj offset)))

(defn ^double get-double
  ([^long addr]
   (.getDouble the-unsafe addr))
  ([^Object obj ^long offset]
   (.getDouble the-unsafe obj offset)))

(defn ^double get-double-volatile
  [^Object obj ^long offset]
  (.getDoubleVolatile the-unsafe obj offset))

(defn ^float get-float
  ([^long addr]
   (.getFloat the-unsafe addr))
  ([^Object obj ^long offset]
   (.getFloat the-unsafe obj offset)))

(defn ^float get-float-volatile
  [^Object obj ^long offset]
  (.getFloatVolatile the-unsafe obj offset))

(defn ^int get-int
  ([^long addr]
   (.getInt the-unsafe addr))
  ([^Object obj ^long offset]
   (.getInt the-unsafe obj offset)))

(defn ^int get-int-volatile
  [^Object obj ^long offset]
  (.getIntVolatile the-unsafe obj offset))

(defn ^int get-load-average
  [loadavg nelms]
  (let [loadavg (doubles loadavg)
        nelms   (int nelms)]
    (.getLoadAverage the-unsafe loadavg nelms)))

(defn ^long get-long
  ([^long addr]
   (.getLong the-unsafe addr))
  ([^Object obj ^long offset]
   (.getLong the-unsafe obj offset)))

(defn ^long get-long-volatile
  [^Object obj ^long offset]
  (.getLongVolatile the-unsafe obj offset))

(defn ^Object get-obj
  [^Object obj ^long offset]
  (.getObject the-unsafe obj offset))

(defn get-obj-volatile
  [^Object obj ^long offset]
  (.getObjectVolatile the-unsafe obj offset))

(defn ^short get-short
  ([^long addr]
   (.getShort the-unsafe addr))
  ([^Object obj ^long offset]
   (.getShort the-unsafe obj offset)))

(defn ^short get-short-volatile
  [^Object obj ^long offset]
  (.getShortVolatile the-unsafe obj offset))

(defn unsafe-monitor-enter
  [^Object obj]
  (.monitorEnter the-unsafe obj))

(defn unsafe-monitor-exit
  [^Object obj]
  (.monitorExit the-unsafe obj))

(defn ^long object-field-offset
  [^Field f]
  (.objectFieldOffset the-unsafe f))

(defn ^int page-size []
  (.pageSize the-unsafe))

(defn park
  [absolute? ^long ptime]
  (let [absolute? (boolean absolute?)]
    (.park the-unsafe absolute? ptime)))

(defn put-address
  [^long addr ^long x]
  (.putAddress the-unsafe addr x))

(defn put-boolean
  [^Object obj ^long offset x]
  (let [x (boolean x)]
    (.putBoolean the-unsafe obj offset x)))

(defn put-boolean-volatile
  [^Object obj ^long offset x]
  (let [x (boolean x)]
    (.putBooleanVolatile the-unsafe obj offset x)))

(defn put-byte
  ([^long addr x]
   (let [x (byte x)]
     (.putByte the-unsafe addr x)))
  ([^Object obj ^long offset x]
   (let [x (byte x)]
     (.putByte the-unsafe obj offset x))))

(defn put-byte-volatile
  [^Object obj ^long offset x]
  (let [x (byte x)]
    (.putByteVolatile the-unsafe obj offset x)))

(defn put-char
  ([^long addr x]
   (let [x (char x)]
     (.putChar the-unsafe addr x)))
  ([^Object obj ^long offset x]
   (let [x (char x)]
     (.putChar the-unsafe obj offset x))))

(defn put-char-volatile
  [^Object obj ^long offset x]
  (let [x (char x)]
    (.putCharVolatile the-unsafe obj offset x)))

(defn put-double
  ([^long addr ^double x]
   (.putDouble the-unsafe addr x))
  ([^Object obj ^long offset ^double x]
   (.putDouble the-unsafe obj offset x)))

(defn put-double-volatile
  [^Object obj ^long offset ^double x]
  (.putDoubleVolatile the-unsafe obj offset x))

(defn put-float
  ([^long addr x]
   (let [x (float x)]
     (.putFloat the-unsafe addr x)))
  ([^Object obj ^long offset x]
   (let [x (float x)]
     (.putFloat the-unsafe obj offset x))))

(defn put-float-volatile
  [^Object obj ^long offset x]
  (let [x (float x)]
    (.putFloatVolatile the-unsafe obj offset x)))

(defn put-int
  ([^long addr x]
   (let [x (int x)]
     (.putInt the-unsafe addr x)))
  ([^Object obj ^long offset x]
   (let [x (int x)]
     (.putInt the-unsafe obj offset x))))

(defn put-int-volatile
  [^Object obj ^long offset x]
  (let [x (int x)]
    (.putIntVolatile the-unsafe obj offset x)))

(defn put-long
  ([^long addr ^long x]
   (.putLong the-unsafe addr x))
  ([^Object obj ^long offset ^long x]
   (.putLong the-unsafe obj offset x)))

(defn put-long-volatile
  [^Object obj ^long offset ^long x]
  (.putLongVolatile the-unsafe obj offset x))

(defn put-obj
  [^Object obj ^long offset ^Object x]
  (.putObject the-unsafe obj offset x))

(defn put-obj-volatile
  [^Object obj ^long offset ^Object x]
  (.putObjectVolatile the-unsafe obj offset x))

(defn put-ordered-int
  [^Object obj ^long offset x]
  (let [x (int x)]
    (.putOrderedInt the-unsafe obj offset x)))

(defn put-ordered-long
  [^Object obj ^long offset ^long x]
  (.putOrderedLong the-unsafe obj offset x))

(defn put-ordered-obj
  [^Object obj ^long offset ^Object x]
  (.putOrderedObject the-unsafe obj offset x))

(defn put-short
  ([^long addr x]
   (let [x (short x)]
     (.putShort the-unsafe addr x)))
  ([^Object obj ^long offset x]
   (let [x (short x)]
     (.putShort the-unsafe obj offset x))))

(defn put-short-volatile
  [^Object obj ^long offset x]
  (let [x (short x)]
    (.putShortVolatile the-unsafe obj offset x)))

(defn ^long reallocate-memory
  [^long addr ^long b]
  (.reallocateMemory the-unsafe addr b))

(defn set-memory
  ([^long addr ^long bs value]
   (let [value (byte value)]
     (.setMemory the-unsafe addr bs value)))
  ([^Object obj ^long offset ^long bs value]
   (let [value (byte value)]
     (.setMemory the-unsafe obj offset bs value))))

(defn ^Object static-field-base
  [^Field f]
  (.staticFieldBase the-unsafe f))

(defn ^long static-field-offset
  [^Field f]
  (.staticFieldOffset the-unsafe f))

(defn throw-exception
  [^Throwable e]
  (.throwException the-unsafe e))

(defn ^boolean try-monitor-enter
  [^Object obj]
  (.tryMonitorEnter the-unsafe obj))

(defn unpark
  [^Thread thread]
  (.unpark the-unsafe thread))
