(ns culture.facts
  "Country-level regional-culture catalog for Israel (ISR) -- national
  dishes, protected products, beverages, crafts, festivals and heritage
  sites, per ADR-2607171400 addendum 2 (cloud-itonami-municipality-
  culture-catalog Wave 1, in com-junkawasaki/root). Sibling namespace to
  `marketentry.facts` / `statute.facts` (ADR-2607141700); city-level
  counterparts live in the cloud-itonami-municipality-* repos.

  Catalog is keyed by UPPERCASE ISO3 (mirrors `statute.facts`); entries
  carry no :culture/municipality (that attribute is city-level only).

  Every entry cites a source URL that was actually fetched and read on
  :culture/retrieved-at -- never fabricated. Summaries state only what the
  cited source confirms. An item not in this table has NO spec-basis, full
  stop; extend `catalog`, do not invent an id/url.")

(def catalog
  "iso3 -> vector of culture entries."
  {"ISR"
   [{:culture/id "isr.dish.falafel"
     :culture/name "Falafel"
     :culture/country "ISR"
     :culture/kind :dish
     :culture/summary "Deep-fried fritter of ground fava beans or chickpeas that most likely originated in Egypt; the chickpea version is a popular street food adopted into Israeli cuisine and called a national dish of Israel, a designation contested by Palestinians and other Arabs."
     :culture/url "https://en.wikipedia.org/wiki/Falafel"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "isr.dish.shakshouka"
     :culture/name "Shakshouka"
     :culture/country "ISR"
     :culture/kind :dish
     :culture/summary "Maghrebi dish of eggs poached in a spiced tomato sauce, brought to Israel by Maghrebi Jewish immigrants in the 1950s and now a popular breakfast and evening meal throughout the country."
     :culture/url "https://en.wikipedia.org/wiki/Shakshouka"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "isr.dish.sabich"
     :culture/name "Sabich"
     :culture/country "ISR"
     :culture/kind :dish
     :culture/summary "Israeli street-food sandwich of fried eggplant, hard-boiled eggs, salad and tahini, created in 1961 in Ramat Gan by Iraqi Jewish immigrant Sabich Tzvi Halabi from ingredients of the Iraqi Jewish Shabbat breakfast."
     :culture/url "https://en.wikipedia.org/wiki/Sabich"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "isr.dish.ptitim"
     :culture/name "Ptitim"
     :culture/country "ISR"
     :culture/kind :dish
     :culture/summary "Toasted grain-shaped wheat pasta, also called Israeli couscous, created in Israel in 1953 as a rice substitute during the austerity period at prime minister David Ben-Gurion's request."
     :culture/url "https://en.wikipedia.org/wiki/Ptitim"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "isr.dish.israeli-salad"
     :culture/name "Israeli salad"
     :culture/country "ISR"
     :culture/kind :dish
     :culture/summary "Chopped salad of finely diced tomato, onion, cucumber and peppers, a standard accompaniment to Israeli meals; it traces back to the Turkish çoban salatası and was adopted by Jewish immigrants to the Levant in the late 19th century."
     :culture/url "https://en.wikipedia.org/wiki/Israeli_salad"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "isr.product.jaffa-orange"
     :culture/name "Jaffa orange"
     :culture/country "ISR"
     :culture/kind :product
     :culture/summary "Orange variety with few seeds and tough skin developed by Arab farmers in mid-19th-century Ottoman Palestine and named after the city of Jaffa from which it was first exported; today cultivated across the region including Israel."
     :culture/url "https://en.wikipedia.org/wiki/Jaffa_orange"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "isr.beverage.israeli-wine"
     :culture/name "Israeli wine"
     :culture/country "ISR"
     :culture/kind :beverage
     :culture/summary "Wine produced in the Land of Israel since biblical times; the modern industry was founded in the late 19th century by Baron Edmond James de Rothschild through the Carmel Winery."
     :culture/url "https://en.wikipedia.org/wiki/Israeli_wine"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "isr.festival.yom-haatzmaut"
     :culture/name "Independence Day"
     :culture/name-local "Yom Ha'atzmaut"
     :culture/country "ISR"
     :culture/kind :festival
     :culture/summary "Israel's national day commemorating the Israeli Declaration of Independence of 14 May 1948, observed on the 5th of Iyar with ceremonies, family celebrations and fireworks."
     :culture/url "https://en.wikipedia.org/wiki/Independence_Day_(Israel)"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "isr.heritage.masada"
     :culture/name "Masada"
     :culture/country "ISR"
     :culture/kind :heritage
     :culture/summary "Ancient mountain-top fortress in the Judaean Desert built in the first century BC, used by Jewish rebels in the First Jewish-Roman War; a UNESCO World Heritage Site inscribed in 2001."
     :culture/url "https://en.wikipedia.org/wiki/Masada"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "isr.heritage.white-city-tel-aviv"
     :culture/name "White City of Tel Aviv"
     :culture/country "ISR"
     :culture/kind :heritage
     :culture/summary "Collection of over 4,000 Bauhaus and International Style buildings from the 1930s in Tel Aviv, built by Jewish architects who fled Nazi Germany; a UNESCO World Heritage Site inscribed in 2003."
     :culture/url "https://en.wikipedia.org/wiki/White_City_(Tel_Aviv)"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}]})

(defn spec-basis [iso3] (get catalog iso3))

(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s)
         missing (remove catalog iso3s)]
     {:requested (count iso3s)
      :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note (str "cloud-itonami-iso3166-isr culture catalog "
                 "(ADR-2607171400 addendum 2, Wave 1): " (count (get catalog "ISR"))
                 " ISR entries, each with a fetched-and-read citation. "
                 "Extend `culture.facts/catalog`, never fabricate an id/url.")})))

(defn by-kind [iso3 kind]
  (filterv #(= (:culture/kind %) kind) (spec-basis iso3)))
