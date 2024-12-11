(ns tst.demo.core
  (:use tupelo.core tupelo.test)
  (:require
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as stest]
    [clojure.string :as str]
    [clojure.test.check.generators :as tcgen]
    [tupelo.spec :as tsp]
    )
  (:import
    [java.util Date]
    [java.time Instant]
    ))

(verify
  ; s/valid? returns true/false
  (is= true (s/valid? even? 4)) ; NOTE: normally just use (is   (s/valid? ...))
  (is= false (s/valid? even? 5)) ; NOTE: normally just use (isnt (s/valid? ...))

  ; s/conform returns the conformed value or :clojure.spec.alpha/invalid
  (is= (s/conform even? 4) 4)
  (is= (s/conform even? 5) :clojure.spec.alpha/invalid)

  (s/check-asserts true) ; *** enable asserts ***
  (is (s/check-asserts?)) ; => normally false
  (is= 4 (s/assert even? 4))
  (throws? (s/assert even? 5))

  ; any function can serve as a predicate
  (is (s/valid? nil? nil))
  (is (s/valid? string? "abc"))
  (is (s/valid? #(< % 5) 3))
  (isnt (s/valid? #(< % 5) 9))

  ; both Date and Instant are a valid "instant" to clojure
  (is (s/valid? inst? (Date.)))
  (is (s/valid? inst? (Instant/now)))

  ; a set is also a predicate function
  (is (s/valid? #{:club :diamond :heart :spade} :club))
  (isnt (s/valid? #{:club :diamond :heart :spade} 42))
  (is (s/valid? #{42 43 44} 42))

  (let [ki-spec (s/map-of keyword? int?)] ; map only
    ; s/valid? returns true/false
    (is (s/valid? ki-spec {:a 1 :b 2}))
    (isnt (s/valid? ki-spec {:a 1 :b "hello"}))

    ; s/conform returns the conformed value or :clojure.spec.alpha/invalid
    (is= (s/conform ki-spec {:a 1 :b 2}) {:a 1 :b 2})
    (is= (s/conform ki-spec {:a 1 :b "hello"}) :clojure.spec.alpha/invalid))

  (let [int-coll-spec (s/coll-of int?)] ; sequence or set
    (is (s/valid? int-coll-spec [1 2 3 4]))
    (is (s/valid? int-coll-spec #{1 2 3 4}))
    (isnt (s/valid? int-coll-spec #{1 :b 3})))

  (let [ints-4  [1 2 3 4]
        ints-3  [1 3 3]
        mixed-4 [1 2 3 :d]]
    (is (s/valid? (s/coll-of int?) ints-4))
    (isnt (s/valid? (s/coll-of int?) mixed-4))
    (is (s/valid? (s/coll-of int? :kind vector?) ints-4))
    (isnt (s/valid? (s/coll-of int? :kind list?) ints-4))

    (is (s/valid? (s/coll-of int? :min-count 2) ints-4))
    (isnt (s/valid? (s/coll-of int? :min-count 5) ints-4))
    (is (s/valid? (s/coll-of int? :min-count 2 :max-count 5) ints-4))
    (isnt (s/valid? (s/coll-of int? :min-count 2 :max-count 3) ints-4))

    ; Uses optional `:distinct` kwarg. See api docs for s/every:
    ;   https://clojure.github.io/spec.alpha/clojure.spec.alpha-api.html#clojure.spec.alpha/every
    (is (s/valid? (s/coll-of int? :distinct true) ints-4))
    (isnt (s/valid? (s/coll-of int? :distinct true) ints-3))
    (is (s/valid? (s/coll-of int? :distinct false) ints-3))
    (is (s/valid? (s/coll-of int?) ints-3)) ; :distinct false is the default

    ; s/every is the "sampling" version of s/coll-of
    (let [ints-4  [1 2 3 4]
          ints-3  [1 3 3]
          mixed-4 [1 2 3 :d]]
      (is (s/valid? (s/every int?) ints-4))
      (isnt (s/valid? (s/every int?) mixed-4))
      (is (s/valid? (s/every int? :kind vector?) ints-4)) ; uses optional kwarg `:kind`
      (isnt (s/valid? (s/every int? :kind list?) ints-4)) ; uses optional kwarg `:kind`

      ; uses optional kwargs `:min-count` & `:max-count`
      (is (s/valid? (s/every int? :min-count 2) ints-4))
      (isnt (s/valid? (s/every int? :min-count 5) ints-4))
      (is (s/valid? (s/every int? :min-count 2 :max-count 5) ints-4))
      (isnt (s/valid? (s/every int? :min-count 2 :max-count 3) ints-4)))))

(verify
  ; defined & use named specs
  (s/def ::date inst?)
  (s/def ::suit #{:club :diamond :heart :spade})
  (is (s/valid? ::date (Date.)))
  (is (s/valid? ::suit :club))

  ; preds for "and" specs don't need names (3 preds here)
  (s/def ::big-even (s/and
                      int? even? #(< 1000 %)))
  (is (s/valid? ::big-even 2222))
  (isnt (s/valid? ::big-even 10))
  (isnt (s/valid? ::big-even :foo))

  ; for "and", conformed value is unchanged
  (is= (s/conform ::big-even 2222) 2222)

  ; preds for "or" specs need a kw name (2 pairs of name+pred here)
  (s/def ::name-or-id (s/or
                        :name string?
                        :id int?))
  (is (s/valid? ::name-or-id "abc"))
  (is (s/valid? ::name-or-id 100))
  (isnt (s/valid? ::name-or-id :foo))

  ; for "or", conformed value is wrapped in a tuple with the pred name
  (is= (s/conform ::name-or-id "abc") [:name "abc"])
  (is= (s/conform ::name-or-id 100) [:id 100])

  (isnt (s/valid? string? nil)) ; preds normally don't accept `nil`
  (is (s/valid? (s/nilable string?) nil)) ; unless wrap pred with `s/nilable`

  ;(is= (s/explain-data ::name-or-id :foo) ; #todo cleanup this
  ;  #:clojure.spec.alpha{:problems
  ;                              '({:path [:name], ; ### NOTE need to quote list ***
  ;                                 :pred clojure.core/string?,
  ;                                 :val  :foo,
  ;                                 :via  [:tst.tupelo.x.spec/name-or-id],
  ;                                 :in   []}
  ;                                 {:path [:id],
  ;                                  :pred clojure.core/int?,
  ;                                  :val  :foo,
  ;                                  :via  [:tst.tupelo.x.spec/name-or-id],
  ;                                  :in   []}),
  ;                       :spec  :tst.tupelo.x.spec/name-or-id,
  ;                       :value :foo})
  )

(verify
  ; define specs for individual values
  (def email-regex #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")
  (s/def ::email-type (s/and string? #(re-matches email-regex %))) ; can use any predicate fn
  (s/def ::acct-id int?)
  (s/def ::first-name string?)
  (s/def ::last-name string?)
  (s/def ::email ::email-type)

  ; Note `:::qual/person` spec forces use of namespaced keywords in maps.
  ; Also, note that name of value spec determines map key.
  (s/def :qual/person (s/keys
                        :req [::first-name ::last-name ::email]
                        :opt [::phone]))

  (is= (s/conform :qual/person {::first-name "Elon" ::last-name "Musk" ::email "elon@example.com"})
    {::email      "elon@example.com",
     ::first-name "Elon",
     ::last-name  "Musk"})

  (isnt (s/valid? :qual/person {::first-name "Elon"}))
  (isnt (s/valid? :qual/person {::first-name "Elon" ::last-name "Musk" ::email "n/a"}))

  ; NOTE:  we cannot avoid mis-spellings of keys like ::phony (instead of ::phone)
  ; or arbitrary additions like ::snailmail
  (is (s/valid? :qual/person {::first-name "Elon" ::last-name "Musk" ::email "elon@example.com"
                              ::phony      "714-555-1234" ::snailmail "3rd door on left"}))

  ; Note `:unqual/person` spec allows maps to use *unqualified* keywords. However, it is
  ; build using *qualified* keywords
  (s/def :unqual/person (s/keys
                          :req-un [::first-name ::last-name ::email]
                          :opt-un [::phone]))
  (is= (s/conform :unqual/person {:first-name "Elon" :last-name "Musk" :email "elon@example.com"})
    {:first-name "Elon", :last-name "Musk", :email "elon@example.com"})

  ; a clojure record is like a a map with non-namespaced keywords
  (defrecord Person [first-name last-name email phone])
  (is (val=         ; <= needed to compare a map & a record
        (s/conform :unqual/person (->Person "Elon" "Musk" "elon@example.com" nil)) ; returns a Person record
        {:first-name "Elon", :last-name "Musk", :email "elon@example.com", :phone nil})))

(verify
  (s/def ::id keyword?)
  (s/def ::host string?)
  (s/def ::port number?)
  ; `s/keys*` yields a spec that takes a sequence of kwargs (key/value pairs in a sequence) instead of a map
  (s/def ::server (s/keys*
                    :req [::id ::host]
                    :opt [::port]))
  (is= (s/conform ::server [::id :s1 ::host "example.com" ::port 5555]) ; seq as input instead of a map
    {::id :s1, ::host "example.com", ::port 5555}))

(verify             ; can merge 2 specs to generate a combined spec
  (s/def :animal/kind string?)
  (s/def :animal/says string?)
  (s/def :animal/common (s/keys :req [:animal/kind :animal/says])) ; common keys for any animal
  (s/def :dog/tail? boolean?)
  (s/def :dog/breed string?)
  (s/def :animal/dog (s/merge :animal/common
                       (s/keys :req [:dog/tail? :dog/breed]))) ; merge in additional keys to create dog spec
  (is (s/valid? :animal/dog {:animal/kind "dog"
                             :animal/says "woof"
                             :dog/tail?   true
                             :dog/breed   "retriever"})))

(verify
  (s/def :event/type keyword?)
  (s/def :event/timestamp int?)
  (s/def :search/url string?)
  (s/def :error/message string?)
  (s/def :error/code int?)

  ; define a multi-method to return 1 of 2 specs
  (defmulti event-type :event/type)
  (defmethod event-type :event/search [_]
    (s/keys :req [:event/type :event/timestamp :search/url]))
  (defmethod event-type :event/error [_]
    (s/keys :req [:event/type :event/timestamp :error/message :error/code]))

  ; define a multi-spec using the above multi-method
  (s/def :event/event (s/multi-spec event-type :event/type))

  (is (s/valid? :event/event {:event/type      :event/search
                              :event/timestamp 1463970123000
                              :search/url      "https://clojure.org"}))
  (is (s/valid? :event/event {:event/type      :event/error
                              :event/timestamp 1463970123000
                              :error/message   "Invalid host"
                              :error/code      500}))

  ;(is= (s/explain-data :event/event {:event/type :event/restart}) ; #todo finish this example
  ;  #:clojure.spec.alpha{:problems [{:path   [:event/restart],
  ;                                   :pred   'tst.tupelo.x.spec/event-type,
  ;                                   :val    #:event{:type :event/restart},
  ;                                   :reason "no method",
  ;                                   :via    [:event/event],
  ;                                   :in     []}],
  ;                       :spec     :event/event,
  ;                       :value    #:event{:type :event/restart}})

  (do ; See guide:  https://clojure.org/guides/spec#_collections
    (is= (s/conform (s/coll-of keyword?) [:a :b :c]) [:a :b :c])
    (is= (s/conform (s/coll-of number?) [1 2 3]) [1 2 3])

    (s/def ::vnum3 (s/coll-of number? :kind vector? :count 3 :distinct true :into #{}))
    (is= (s/conform ::vnum3 [1 2 3]) #{1 2 3})
    (is= (s/conform ::vnum3 #{1 2 3}) :clojure.spec.alpha/invalid)
    (is= (s/conform ::vnum3 [1 1 1]) :clojure.spec.alpha/invalid)
    (is= (s/conform ::vnum3 [1 2 :a]) :clojure.spec.alpha/invalid)

    (s/def ::point (s/tuple double? double? double?))
    (is= (s/conform ::point [1.5 2.0 3.1]) [1.5 2.0 3.1])

    (s/def ::scores (s/map-of string? int?)) ; every entry is string -> int
    (is= (s/conform ::scores {"Sally" 1000 "joe" 500}) {"Sally" 1000 "joe" 500})))

(verify             ; see guide:  https://clojure.org/guides/spec#_sequences
  (s/def ::ingredient (s/cat ; for a `cat` spec each predicate needs a kw name
                        :quantity number?
                        :unit keyword?))
  (is (s/valid? ::ingredient [2 :teaspoon]))
  (is= (s/conform ::ingredient [2 :teaspoon])
    {:quantity 2, :unit :teaspoon}) ; conform outputs as mape with [name value] pairs

  (s/def ::seq-of-keywords (s/* keyword?))
  (is= (s/conform ::seq-of-keywords [:a :b :c]) [:a :b :c])

  (s/def ::odds-then-maybe-even (s/cat
                                  :odds (s/+ odd?)
                                  :even (s/? even?)))
  (is (s/valid? ::odds-then-maybe-even [1 3 5 66]))
  (is (s/valid? ::odds-then-maybe-even [1 3 5]))
  (isnt (s/valid? ::odds-then-maybe-even [1 3 66 5]))
  (isnt (s/valid? ::odds-then-maybe-even [2]))

  ; ::opts are alternating pairs of keyword & boolean
  (s/def ::opts (s/* (s/cat :opt keyword? :val boolean?)))
  (is= (s/conform ::opts [:silent false :verbose true])
    [{:opt :silent, :val false}
     {:opt :verbose, :val true}])

  ; a property/value pair. prop is a string. value can be string or boolean
  (s/def ::config (s/* (s/cat
                         :prop string?
                         :val (s/alt :s string? :b boolean?))))
  (is= (s/conform ::config ["-server" "foo" "-verbose" true "-user" "joe"])
    [{:prop "-server", :val [:s "foo"]}
     {:prop "-verbose", :val [:b true]}
     {:prop "-user", :val [:s "joe"]}])

  (is= (s/describe ::seq-of-keywords)
    '(* keyword?))
  (is= (s/describe ::odds-then-maybe-even)
    '(cat :odds (+ odd?) :even (? even?)))
  (is= (s/describe ::opts)
    '(* (cat :opt keyword? :val boolean?)))

  ; regex can also include an arbitrary predicate fn
  (s/def ::even-strings (s/& (s/* string?) #(even? (count %))))
  (is (s/valid? ::even-strings ["a" "b"]))
  (is (s/valid? ::even-strings ["a" "b" "c" "d"]))
  (isnt (s/valid? ::even-strings ["a"]))
  (isnt (s/valid? ::even-strings ["a" "b" "c"]))

  ; define a nested regex context via embedded `s/spec` calls
  (s/def ::nested
    (s/cat
      :names-kw #{:names}
      :names (s/spec (s/* string?))
      :nums-kw #{:nums}
      :nums (s/spec (s/* number?))))
  (is= (s/conform ::nested [:names ["a" "b"] :nums [1 2 3]])
    {:names-kw :names, :names ["a" "b"], :nums-kw :nums, :nums [1 2 3]})

  (s/def ::unnested
    (s/cat :names-kw #{:names}
      :names (s/* string?)
      :nums-kw #{:nums}
      :nums (s/* number?)))
  (is= (s/conform ::unnested [:names "a" "b" :nums 1 2 3])
    {:names-kw :names, :names ["a" "b"], :nums-kw :nums, :nums [1 2 3]}))

;-----------------------------------------------------------------------------
; function specs:  https://clojure.org/guides/spec#_specing_functions
(defn ranged-rand
  "Returns random int in range start <= rand < end"
  [start end]
  (+ start (long (rand (- end start)))))

(s/fdef ranged-rand
  :args (s/and
          (s/cat :start int?
            :end int?)
          #(< (:start %) (:end %) 1e9)) ; need add 1e9 limit to avoid integer overflow
  :ret int?
  :fn (s/and #(<= (-> % :args :start) (:ret %))
        #(< (:ret %) (-> % :args :end))))

(verify
  (when true
    (stest/instrument `ranged-rand)
    (throws? Exception (ranged-rand 8 5))
    ; (spyx (stest/check `ranged-rand))  #todo
    ; (spyx (s/exercise-fn `ranged-rand))
    )
  )

;-----------------------------------------------------------------------------
; A game of cards:  https://clojure.org/guides/spec#_a_game_of_cards
(verify
  (def suit? #{:club :diamond :heart :spade}) ; remember sets are predicates
  (def rank? (into #{:jack :queen :king :ace} (thru 2 10))) ; remember sets are predicates
  (def deck (forv [suit suit? rank rank?]
              [rank suit]))
  (is (set/subset? #{[2 :club] [5 :diamond] [:queen :heart]}
        (into #{} deck)))

  (s/def ::card (s/tuple rank? suit?))
  (s/def ::hand (s/* ::card))

  (s/def ::name string?)
  (s/def ::score int?)
  (s/def ::player (s/keys :req [::name ::score ::hand]))
  ; #todo s/keys -> tsp/map-with-keys  tsp/entity-map (really more like an object declaration)

  (s/def ::players (s/* ::player))
  (s/def ::deck (s/* ::card))
  (s/def ::game (s/keys :req [::players ::deck])) ; #todo s/keys -> tsp/map-with-keys tsp/entity-map

  (def kenny {::name  "Kenny Rogers"
              ::score 100
              ::hand  []})
  (is (s/valid? ::player kenny))
  (is (s/valid? ::player {::name  "Kenny Rogers"
                          ::score 100
                          ::hand  [[2 :heart]]}))
  (isnt (s/valid? ::player {::name  "Kenny Rogers"
                            ::score 100
                            ::hand  [[2 :hurts]]}))

  ; Create a generator for ::player, generate a single sample value, and verify it is valid
  (is (s/valid? ::player (gen/generate (s/gen ::player))))

  ; s/generate returns a single entity; s/sample returns multiple entity
  (when false
    (nl)
    (spyx-pretty (gen/generate (s/gen ::player)))
    (nl)
    (spyx-pretty (gen/sample (s/gen ::player)))))

;-----------------------------------------------------------------------------
; examples from: Clojure spec Screencast - Customizing Generators: https://youtu.be/WoFkhE92fqc
(when false         ; #todo finish this

  (s/def ::foo-id (s/and string? #(str/starts-with? % "FOO-")))
  (defn foo-id-gen []
    (->> (s/gen (s/int-in 1 100))
      (gen/fmap #(str "FOO-" %))))


  (verify
    (spy :foo-id (mapv first (s/exercise ::foo-id 10
                               {::foo-id foo-id-gen})))) ; a generator override

  ; Lookup
  (s/def ::lookup (s/map-of keyword? string? :min-count 1))
  (s/def ::lookup-finding-k (s/and (s/cat
                                     :lookup ::lookup
                                     :k keyword?)
                              (fn [{:keys [lookup k]}] (contains? lookup k))))
  (defn lookup-finding-k-gen []
    (gen/bind (s/gen ::lookup)
      #(gen/tuple
         (gen/return %)
         (gen/elements (keys %)))))

  (verify
    (nl)
    (spyx (mapv first (s/exercise ::lookup)))
    (throws? (mapv first (s/exercise ::lookup-finding-k)))
    (spyx (s/exercise ::lookup-finding-k 10 {::lookup-finding-k lookup-finding-k-gen})))

  (defn my-index-of [source tgt] (str/index-of source tgt))
  (defn my-index-of-2 [source tgt] (str/index-of source tgt))
  (defn my-index-of-3 [source tgt] (str/index-of source tgt))
  (defn my-index-of-4 [source tgt] (str/index-of source tgt))

  (s/fdef my-index-of :args (s/cat
                              :source string?
                              :tgt string?))
  ; constructively generate a string and substring
  (def string-and-substring-model (s/cat :prefix string? :match string? :suffix string?))
  (defn gen-string-and-substring [] (gen/fmap
                                      (fn [[prefix match suffix]] [(str prefix match suffix) match])
                                      (s/gen string-and-substring-model)))
  (s/def ::my-index-of-args (s/cat :source string? :tgt string?))
  (s/fdef my-index-of-2 :args (s/spec ::my-index-of-args
                                :gen gen-string-and-substring))

  (defn gen-my-index-of-args []
    (gen/one-of [(gen-string-and-substring)
                 (s/gen ::my-index-of-args)]))
  (s/fdef my-index-of-3 :args (s/spec ::my-index-of-args
                                :gen gen-my-index-of-args))

  (defn gen-string-and-substring-let []
    ; RHS must be tcgen/* generator fns
    (tcgen/let [prefix tcgen/string-alphanumeric
                tgt tcgen/string-alphanumeric
                suffix tcgen/string-alphanumeric]
      (let [search-str (str prefix tgt suffix)
            result     [search-str tgt]]
        result)))
  (defn gen-my-index-of-let []
    (gen/one-of [(gen-string-and-substring-let)
                 ;(s/gen ::my-index-of-args)
                 ]))
  (s/fdef my-index-of-4 :args (s/spec ::my-index-of-args
                                :gen gen-my-index-of-let))

  (verify
    (nl) (spyx (s/exercise-fn `my-index-of))
    (nl) (spyx (s/exercise-fn `my-index-of-2))
    (nl) (spyx (s/exercise-fn `my-index-of-3))
    (nl) (spyx (s/exercise-fn `my-index-of-4)))

  (verify (nl)
    (spyx (tcgen/sample (tcgen/fmap set (tcgen/vector tcgen/nat))))
    (spyx (tcgen/sample (tcgen/fmap
                          (fn [v]
                            [(rand-nth v) v])
                          (tcgen/not-empty (tcgen/vector tcgen/nat)))))))

