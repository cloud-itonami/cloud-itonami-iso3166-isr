(ns marketentry.facts "Israel market-entry catalog.")
(def catalog
  {"ISR" {:name "Israel"
          :owner-authority "Accountant General / Merkava / e-Government procurement"
          :legal-basis "Mandatory Tenders Law"
          :national-spec "Government procurement portals + Company Registrar number"
          :provenance "https://mr.gov.il/"
          :required-evidence ["Company Registrar number record" "procurement registration record" "Registrar extract" "Authorized-representative record"]
          :rep-owner-authority "contracting authorities / Accountant General"
          :rep-legal-basis "Israeli company registration typically required for government awards"
          :rep-provenance "https://mr.gov.il/"
          :corporate-number-owner-authority "Companies Authority / Tax Authority"
          :corporate-number-legal-basis "Company number / tax ID"
          :corporate-number-provenance "https://www.gov.il/"}
   "USA" {:name "United States" :owner-authority "GSA/SAM.gov" :legal-basis "FAR" :national-spec "SAM.gov" :provenance "https://sam.gov/"
          :required-evidence ["EIN record" "SAM.gov registration record" "State business registration record" "SAM UEI verification record"]}
   "DEU" {:name "Germany" :owner-authority "e-Vergabe" :legal-basis "GWB/VgV" :national-spec "e-Vergabe" :provenance "https://www.evergabe-online.de/"
          :required-evidence ["Handelsregister extract" "e-Vergabe registration record" "USt-IdNr record" "Authorized-representative record"]}
   "TUR" {:name "Türkiye" :owner-authority "EKAP" :legal-basis "KİK" :national-spec "EKAP" :provenance "https://ekap.kik.gov.tr/"
          :required-evidence ["VKN record" "EKAP registration" "Trade registry extract" "Authorized-representative record"]}})

(defn spec-basis [iso3] (get catalog iso3))
(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s) missing (remove catalog iso3s)]
     {:requested (count iso3s) :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note "R0 catalog seed"})))
(defn required-evidence-satisfied? [iso3 submitted]
  (when-let [{:keys [required-evidence]} (spec-basis iso3)]
    (= (count required-evidence) (count (filter (set submitted) required-evidence)))))
(defn evidence-checklist [iso3] (:required-evidence (spec-basis iso3) []))
(defn rep-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:rep-owner-authority sb)
      (select-keys sb [:rep-owner-authority :rep-legal-basis :rep-provenance]))))
(defn corporate-number-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:corporate-number-owner-authority sb)
      (select-keys sb [:corporate-number-owner-authority :corporate-number-legal-basis :corporate-number-provenance]))))
