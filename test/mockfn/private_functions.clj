(ns mockfn.private-functions)

(defn- private-inc [a] (inc a))

(defn call-private-inc [x]
  (private-inc x))
