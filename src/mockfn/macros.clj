(ns mockfn.macros
  (:require [mockfn.mock :as mock]))

(defn- as-redefs
  [func->definition]
  (->> func->definition
       (map (fn [[func definition]] [func `(mock/mock ~(:function definition)
                                                      ~definition)]))
       (apply concat)))

(defn- handle-private [func]
  (if (and (seq? func)
           (= 'var (first func)))
    (second func)
    func))

(defn- func->spec
  [bindings]
  (reduce
    (fn [acc [[raw-func & args] ret-val & times-expected]]
      (let [func (handle-private raw-func)]
        (-> acc
            (assoc-in [func :function] raw-func)
            (assoc-in [func :return-values (into [] args)] ret-val)
            (assoc-in [func :times-called (into [] args)] `(atom 0))
            (assoc-in [func :times-expected (into [] args)] (into [] times-expected)))))
    {} bindings))

(defn calling
  "Invoke mocked value as a function instead of returning it"
  [func] (mock/->Calling func))

(defmacro providing
  "Mocks functions."
  [bindings & body]
  `(with-redefs ~(->> bindings (partition 2) func->spec as-redefs)
     ~@body))

(defmacro verifying
  "Mocks functions and verifies calls."
  [bindings & body]
  (let [specs# (->> bindings (partition 3) func->spec)]
    `(with-redefs ~(as-redefs specs#)
       ~@body
       (doseq [mock# (keys ~specs#)] (mock/verify mock#)))))
