;; -*-scheme-*-
;; 
;; This file was copied from the file txf.scm by  Richard -Gilligan- Uschold
;;
;; Originally, these were meant to hold the codes for the US tax TXF
;; format. I modified this heavily so that it might become useful for
;; the German Umsatzsteuer-Voranmeldung. 
;; 
;; This file holds all the Kennzahlen for the
;; Umsatzsteuer-Voranmeldung and their explanations, which can be
;; assigned to particular accounts via the "Edit -> Tax options"
;; dialog. The report in taxtxf-de_DE.scm then will extract the
;; numbers for these Kennzahlen from the actual accounts for a given
;; time period, and will write it to some XML file as required by
;; e.g. the Winston software
;; http://www.felfri.de/winston/schnittstellen.htm
;;
(define (gnc:txf-get-payer-name-source categories code)
  (gnc:txf-get-code-info categories code 0))
(define (gnc:txf-get-form categories code)
  (gnc:txf-get-code-info categories code 1))
(define (gnc:txf-get-description categories code)
  (gnc:txf-get-code-info categories code 2))
(define (gnc:txf-get-format categories code)
  (gnc:txf-get-code-info categories code 3))
(define (gnc:txf-get-multiple categories code)
  (gnc:txf-get-code-info categories code 4))
(define (gnc:txf-get-category-key categories code)
  (gnc:txf-get-code-info categories code 5))
(define (gnc:txf-get-help categories code)
  (let ((pair (assv code txf-help-strings)))
    (if pair
        (cdr pair)
        "No help available.")))

(define (gnc:txf-get-codes categories)
  (map car categories))

;;;; Private

(define (gnc:txf-get-code-info categories code index)
  (let ((category (assv code categories)))
    (and category
         (vector-ref (cdr category) index))))

(define txf-help-categories
  (list
   (cons 'H000 #(current "help" "Name of Current account is exported." 0 #f ""))
   (cons 'H002 #(parent "help" "Name of Parent account is exported." 0 #f ""))
   (cons 'H003 #(not-impl "help" "Not implemented yet, Do NOT Use!" 0 #f ""))))

;; We use several formats; nr. 1 means Euro+Cent, nr. 2 means only full Euro

;; Also, we abuse the "category-key" for now to store the Kennzahl as
;; well. We are not yet sure what to use the "form" field for.

;; Format: (name-source  form  description  format  multiple  category-key)
(define txf-income-categories
  (list
   (cons 'N000 #(none "" "Nur zur Voransicht im Steuer-Bericht -- kein Export" 0 #f ""))

   (cons 'K35 #(none "35" "Umsätze, die anderen Steuersätzen unterliegen (Bemessungsgrundlage)" 2 #f "35"))
   (cons 'K36 #(none "36" "Umsätze, die anderen Steuersätzen unterliegen (Steuer)" 1 #f "36"))
   (cons 'K39 #(none "39" "Anrechnung (Abzug) der festgesetzten Sondervorauszahlung für Dauerfristverlängerung" 1 #f "39"))
   (cons 'K41 #(none "41" "Innergemeinschaftliche Lieferungen an Abnehmer mit USt-IdNr. " 2 #f "41"))
   (cons 'K42 #(none "42" "Lieferungen des ersten Abnehmers (§25b Abs. 2 UStG) bei innergemeinschaftlichen Dreiecksgeschäften" 2 #f "42"))
   (cons 'K43 #(none "43" "Weitere steuerfreie Umsätze mit Vorsteuerabzug" 2 #f "43"))
   (cons 'K44 #(none "44" "Innergemeinschaftliche Lieferungen neuer Fahrzeuge an Abnehmer ohne USt-IdNr" 2 #f "44"))
   (cons 'K45 #(none "45" "Im Inland nicht steuerbare Umsätze" 2 #f "45"))
   (cons 'K48 #(none "48" "Steuerfreie Umsätze ohne Vorsteuerabzug" 2 #f "48"))
   (cons 'K49 #(none "49" "Innergemeinschaftliche Lieferungen neuer Fahrzeuge außerhalb eines Unternehmens" 2 #f "49"))
   (cons 'K51 #(none "51" "Steuerpflichtige Umsätze, Steuersatz 16 v.H." 2 #f "51"))
   (cons 'K60 #(none "60" "Umsätze, für die der Leistungsempfänger die Steuer nach §13b Abs 2 UStG schuldet" 2 #f "60"))
   (cons 'K76 #(none "76" "Umsätze, für die eine Steuer nach § 24 UStG zu entrichten ist (Bemessungsgrundlage)" 2 #f "76"))
   (cons 'K77 #(none "77" "Lieferungen in das übrige Gemeinschaftsgebiet an Abnehmer mit USt-IdNr." 2 #f "77"))
   (cons 'K80 #(none "80" "Umsätze, für die eine Steuer nach § 24 UStG zu entrichten ist (Steuer)" 1 #f "80"))
   (cons 'K81 #(none "81" "Steuerpflichtige Umsätze, Steuersatz 19 v.H." 2 #f "81"))
   (cons 'K83 #(none "83" "Verbleibende Umsatzsteuer-Vorauszahlung" 1 #f "83"))
   (cons 'K86 #(none "86" "Steuerpflichtige Umsätze, Steuersatz 7 v.H." 2 #f "86"))
   (cons 'K89 #(none "89" "Steuerpflichtige innergemeinschaftliche Erwerbe zum Steuersatz von 16 v.H." 2 #f "89"))
   (cons 'K93 #(none "93" "Steuerpflichtige innergemeinschaftliche Erwerbe zum Steuersatz von 7 v.H." 2 #f "93"))
   (cons 'K97 #(none "97" "Steuerpflichtige innergemeinschaftliche Erwerbe zum Steuersatz von 16 v.H." 2 #f "97"))
   (cons 'K98 #(none "98" "Steuerpflichtige innergemeinschaftliche Erwerbe zu anderen Steuersätzen (Steuer)" 1 #f "98"))

   ))


;; We use several formats; nr. 1 means Euro+Cent, nr. 2 means only full Euro

;; Also, we abuse the "category-key" for now to store the Kennzahl as
;; well. We are not yet sure what to use the "form" field for.

;; Format: (name-source  form  description  format  multiple  category-key)
(define txf-expense-categories
  (list
   (cons 'N000 #(none "" "Nur zur Voransicht im Steuer-Bericht -- kein Export" 0 #f ""))

   (cons 'K52 #(none "52" "Leistungen eines im Ausland ansässigen Unternehmers (Bemessungsgrundlage)" 2 #f "52"))
   (cons 'K53 #(none "53" "Leistungen eines im Ausland ansässigen Unternehmers (Steuer)" 2 #f "52"))
   (cons 'K59 #(none "59" "Vorsteuerabzug für innergemeinschaftliche Lieferungen neuer Fahrzeuge außerhalb eines Unternehmens" 1 #f "59"))
   (cons 'K61 #(none "61" "Vorsteuerbeträge aus dem innergemeinschaftlichen Erwerb von Gegenständen" 1 #f "61"))
   (cons 'K62 #(none "62" "Entrichtete Einfuhrumsatzsteuer" 1 #f "62"))
   (cons 'K63 #(none "63" "Vorsteuerbeträge, die nach allgemeinen Durchschnittssätzen berechnet sind" 1 #f "63"))
   (cons 'K64 #(none "64" "Berichtigung des Vorsteuerabzugs" 1 #f "64"))
   (cons 'K65 #(none "65" "Steuer infolge Wechsels der Besteuerungsart/-form" 1 #f "65"))
   (cons 'K66 #(none "66" "Vorsteuerbeträge aus Rechnungen von anderen Unternehmern" 1 #f "66"))
   (cons 'K67 #(none "67" "Vorsteuerbeträge aus Leistungen im Sinne des $13b Abs. 1 UStG" 1 #f "67"))
   (cons 'K69 #(none "69" "Steuerbeträge, die vom letzten Abnehmer eines innergemeinschaftlichen Dreiecksgeschäfts geschuldet werden" 1 #f "69"))
   (cons 'K73 #(none "73" "Lieferungen sicherungsübereigneter Gegenstände und Umsätze, die unter das GrEStG fallen (Bemessungsgrundlage)" 2 #f "74"))
   (cons 'K74 #(none "74" "Lieferungen sicherungsübereigneter Gegenstände und Umsätze, die unter das GrEStG fallen (Steuer)" 2 #f "74"))
   (cons 'K84 #(none "84" "Bauleistungen eines im Inland ansässigen Unternehmers (Bemessungsgrundlage" 1 #f "84"))
   (cons 'K85 #(none "85" "Bauleistungen eines im Inland ansässigen Unternehmers (Steuer)" 1 #f "85"))
   (cons 'K91 #(none "91" "Steuerfreie innergemeinschaftliche Erwerbe nach §4b UStG" 2 #f "91"))
   (cons 'K94 #(none "94" "Innergemeinschaftliche Erwerbe neuer Fahrzeuge von Lieferern ohne USt-IdNr. (Bemessungsgrundlage)" 2 #f "94"))
   (cons 'K95 #(none "95" "Steuerpflichtige innergemeinschaftliche Erwerbe zu anderen Steuersätzen (Bemessungsgrundlage)" 2 #f "95"))
   (cons 'K96 #(none "96" "Innergemeinschaftliche Erwerbe neuer Fahrzeuge von Lieferern ohne USt-IdNr. (Steuer)" 1 #f "96"))

   ))



;;; Register global options in this book
(define (book-options-generator options)
  (define (reg-option new-option)
    (gnc:register-option options new-option))

  (reg-option
   (gnc:make-string-option
    gnc:*tax-label* gnc:*tax-nr-label*
    "a" (N_ "The electronic tax number of your business") ""))
  )

(gnc-register-kvp-option-generator QOF-ID-BOOK-SCM book-options-generator)
