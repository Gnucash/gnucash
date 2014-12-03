;; -*-scheme-*-
;; 
;; This file was copied from the file txf.scm by  Richard -Gilligan- Uschold
;;
;; Originally, these were meant to hold the codes for the US tax TXF
;; format. Christian Stimming modified this heavily so that it might become useful for
;; the German Umsatzsteuer-Voranmeldung. 
;; Further modifications by:
;;   Jannick Asmus
;;   J. Alex Aycinena
;;   Frank H. Ellenberger
;;   Andreas Köhler
;;   Rolf Leggewie
;; 
;; Der gesamte Inhalt zu den vier Abschnitten der "Umsatzsteuer-Kategorien" wurden im
;; Dezember 2010 für GnuCash Vers. 2.4.0 vollständig überarbeitet und alle Einträge gemäß
;; der "Umsatzsteuer-Voranmeldung 2011" umfassend berichtigt und komplettiert von FJSW - Franz Stoll
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

(use-modules (gnucash app-utils))

(define txf-tax-entity-types
  (list
   (cons 'Ind #("Individual, Joint, etc." "Umsatzsteuer-Kennzahlen"))
   (cons 'Other #("None" "Keine Steuerberichtsoptionen vorhanden"))))

(define (gnc:tax-type-txf-get-code-info tax-entity-types type-code index)
  (if (assv type-code tax-entity-types)
      (let ((tax-entity-type (assv type-code tax-entity-types)))
           (and tax-entity-type
                (vector-ref (cdr tax-entity-type) index)))
      #f))

(define (gnc:txf-get-tax-entity-type type-code)
  (gnc:tax-type-txf-get-code-info txf-tax-entity-types type-code 0))

(define (gnc:txf-get-tax-entity-type-description type-code)
  (gnc:tax-type-txf-get-code-info txf-tax-entity-types type-code 1))

(define (gnc:txf-get-tax-entity-type-codes)
  (map car txf-tax-entity-types))

(define (gnc:txf-get-payer-name-source categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 0 tax-entity-type))
(define (gnc:txf-get-form categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 1 tax-entity-type))
(define (gnc:txf-get-description categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 2 tax-entity-type))
(define (gnc:txf-get-format categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 3 tax-entity-type))
(define (gnc:txf-get-multiple categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 4 tax-entity-type))
(define (gnc:txf-get-category-key categories code tax-entity-type)
  (gnc:txf-get-code-info categories code 5 tax-entity-type))
(define (gnc:txf-get-line-data categories code tax-entity-type)
  (if (assv (string->symbol tax-entity-type) categories)
      (let* ((tax-entity-codes (cdr (assv (string->symbol tax-entity-type)
                                          categories)))
             (category (if (assv code tax-entity-codes)
                           (assv code tax-entity-codes)
                           #f)))
            (if (or (not category) (< (vector-length (cdr category)) 7))
                #f
                (gnc:txf-get-code-info categories code 6 tax-entity-type)))
      #f))
(define (gnc:txf-get-last-year categories code tax-entity-type)
  (if (assv (string->symbol tax-entity-type) categories)
      (let* ((tax-entity-codes (cdr (assv (string->symbol tax-entity-type)
                                          categories)))
             (category (if (assv code tax-entity-codes)
                           (assv code tax-entity-codes)
                           #f)))
            (if (or (not category) (< (vector-length (cdr category)) 8))
                #f
                (gnc:txf-get-code-info categories code 7 tax-entity-type)))
      #f))

(define (gnc:txf-get-help categories code)
  (let ((pair (assv code txf-help-strings)))
    (if pair
        (cdr pair)
        "Keine Hilfe verfügbar, da nur Gruppenüberschrift.
Diese Kategorie ohne Nummer ==>> N I C H T   V E R W E N D E N !
USt-Kategorien 2011 für GnuCash Vers. 2.4.0 entwickelt und erstellt von: FJSW
Fehlermeldungen + Dankschreiben an: stoll@bomhardt.de")))

(define (gnc:txf-get-codes categories tax-entity-type)
  (if (assv (string->symbol tax-entity-type) categories)
      (let* ((tax-entity-code-list-pair (assv (if (eqv? tax-entity-type "")
                                               'Ind
                                               (string->symbol tax-entity-type))
                                              categories))
             (tax-entity-codes (if tax-entity-code-list-pair
                                   (cdr tax-entity-code-list-pair)
                                   '())))
            (map car tax-entity-codes))
      #f))

;;;; Private

(define (gnc:txf-get-code-info categories code index tax-entity-type)
  (let* ((tax-entity-code-list-pair (assv (if (eqv? tax-entity-type "")
                                              'Ind
                                              (string->symbol tax-entity-type))
                                          categories))
         (tax-entity-codes (if tax-entity-code-list-pair
                               (cdr tax-entity-code-list-pair)
                               '()))
         (category (assv code tax-entity-codes)))
        (if category
            (and category
                 (vector-ref (cdr category) index))
            #f)))

(define txf-help-categories
  (list
   (cons 'H000 #(current "help" "Name des aktuellen Kontos wird exportiert." 0 #f ""))
   (cons 'H002 #(parent "help" "Name des übergeordneten Kontos wird exportiert." 0 #f ""))
   (cons 'H003 #(not-impl "help" "Noch nicht implementiert, NICHT benutzen!" 0 #f ""))))

;; We use several formats; nr. 1 means Euro+Cent, nr. 2 means only full Euro

;; Also, we abuse the "category-key" for now to store the Kennzahl as
;; well. We are not yet sure what to use the "form" field for.

;; Format: (name-source  form  description  format  multiple  category-key)
(define txf-income-categories
 (list
  (cons 'Ind
   (list
   (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))
   (cons 'UST #(none "" "Diese und nachfolgende Kategorien ohne Nummer --> NICHT VERWENDEN !" 0 #f ""))

   (cons 'U00 #(none "" "Lieferungen und sonstige Leistungen (einschließlich unentgeltlicher Wertabgaben)" 0 #f ""))
   (cons 'U10 #(none "" "Steuerfreie Umsätze mit Vorsteuerabzug" 0 #f ""))
   (cons 'U11 #(none "" "Innergemeinschaftliche Lieferungen (§ 4 Nr. 1 Buchst. b UStG)" 0 #f ""))
   (cons 'K41 #(none "41" "an Abnehmer MIT USt-IdNr." 2 #f "41"))
   (cons 'K44 #(none "44" "neuer Fahrzeuge an Abnehmer OHNE USt-IdNr." 2 #f "44"))
   (cons 'K49 #(none "49" "neuer Fahrzeuge außerhalb eines Unternehmens (§ 2a UStG)" 2 #f "49"))
   (cons 'K43 #(none "43" "Weitere steuerfreie Umsätze mit Vorsteuerabzug" 2 #f "43"))
   (cons 'U12 #(none "" "Steuerfreie Umsätze ohne Vorsteuerabzug" 0 #f ""))
   (cons 'K48 #(none "48" "Umsätze nach § 4 Nr. 8 bis 28 UStG" 2 #f "48"))

   (cons 'U20 #(none "" "Steuerpflichtige Umsätze (Lieferungen u. sonst. Leistungen einschl. unentgeltlicher Wertabgaben)" 0 #f ""))
   (cons 'K51 #(none "51" "zum Steuersatz von 16 %  (bis incl. 2006)" 2 #f "51"))
   (cons 'K81 #(none "81" "zum Steuersatz von 19 %" 2 #f "81"))
   (cons 'K86 #(none "86" "zum Steuersatz von  7 %" 2 #f "86"))
   (cons 'K35 #(none "35" "zu anderen Steuersätzen (Erlöse)" 2 #f "35"))
   (cons 'K36 #(none "36" "zu anderen Steuersätzen (MWSt)" 1 #f "36"))
   (cons 'U21 #(none "" "Lieferungen land- und forstwirtschaftlicher Betriebe nach § 24 UStG" 0 #f ""))
   (cons 'K77 #(none "77" "an Abnehmer MIT USt-IdNr." 2 #f "77"))
   (cons 'K76 #(none "76" "Umsätze, für die eine Steuer nach § 24 UStG zu entrichten ist (Erlöse)" 2 #f "76"))
   (cons 'K80 #(none "80" "Umsätze, für die eine Steuer nach § 24 UStG zu entrichten ist (MWSt)" 1 #f "80"))

   (cons 'U30 #(none "" "Innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'U32 #(none "" "Steuerpflichtige innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'K98 #(none "98" "zu anderen Steuersätzen (Gebuchte MWSt)" 1 #f "98"))
   (cons 'K96 #(none "96" "neuer Fahrzeuge von Lieferern OHNE USt-IdNr. zum allgemeinen Steuersatz (Gebuchte MWSt)" 1 #f "96"))

   (cons 'U40 #(none "" "Ergänzende Angaben zu Umsätzen" 0 #f ""))
   (cons 'K42 #(none "42" "Lieferungen des ersten Abnehmers bei innergemeinschaftl. Dreiecksgeschäften (§ 25b Abs. 2 UStG)" 2 #f "42"))
   (cons 'K60 #(none "60" "Steuerpflichtige Umsätze, für die der Leistungsempfänger die Steuer nach § 13b Abs. 5 UStG schuldet" 2 #f "60"))
   (cons 'K21 #(none "21" "Nicht steuerbare sonstige Leistungen gem. § 18b Satz 1 Nr. 2 UStG (Leistungen EU-Land)" 2 #f "21"))
   (cons 'K45 #(none "45" "Übrige nicht steuerbare Umsätze (Leistungsort nicht im Inland)" 2 #f "45"))

   (cons 'U50 #(none "" "Leistungsempfänger als Steuerschuldner (§ 13b UStG)" 0 #f ""))
   (cons 'K47 #(none "47" "Im Inland steuerpflichtige sonstige Leistungen aus EU-Ländern (§13b Abs. 1 UStG) (Gebuchte MWSt)" 1 #f "47"))
   (cons 'K53 #(none "53" "Andere Leistungen eines im Ausland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 1 und 5 UStG) (Gebuchte MWSt)" 1 #f "53"))
   (cons 'K74 #(none "74" "Lieferungen sicherheitsübereigneter Gegenstände und Umsätze, die unter das GrEStG fallen (Gebuchte MWSt)" 1 #f "74"))
   (cons 'K85 #(none "85" "Andere Umsätze eines im Inland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 4 und Nr. 6 bis 9 UStG) (Gebuchte MWSt)" 1 #f "85"))

   (cons 'U70 #(none "" "Andere Steuerbeträge" 0 #f ""))
   (cons 'K65 #(none "65" "Steuer infolge Wechsels der Besteuerungsform sowie Nachsteuer auf versteuerte Anzahlungen u. ä." 1 #f "65"))
   (cons 'K69 #(none "69" "In Rechnungen unrichtig oder unberechtigt ausgewiesene Steuerbeträge (§ 14c UStG)" 1 #f "69"))

   )
  )
  (cons 'Other
   (list
    (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))
   )
  )
))


;; We use several formats; nr. 1 means Euro+Cent, nr. 2 means only full Euro

;; Also, we abuse the "category-key" for now to store the Kennzahl as
;; well. We are not yet sure what to use the "form" field for.

;; Format: (name-source  form  description  format  multiple  category-key)
(define txf-expense-categories
 (list
  (cons 'Ind
   (list
   (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))
   (cons 'UST #(none "" "Diese und nachfolgende Kategorien ohne Nummer --> NICHT VERWENDEN !" 0 #f ""))

   (cons 'U30 #(none "" "Innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'U31 #(none "" "Steuerfreie innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'K91 #(none "91" "Erwerbe nach § 4b UStG" 2 #f "91"))
   (cons 'U32 #(none "" "Steuerpflichtige innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'K89 #(none "89" "zum Steuersatz von 19 %" 2 #f "89"))
   (cons 'K93 #(none "93" "zum Steuersatz von  7 %" 2 #f "93"))
   (cons 'K95 #(none "95" "zu anderen Steuersätzen (Aufwand)" 2 #f "95"))
   (cons 'K94 #(none "94" "neuer Fahrzeuge von Lieferern OHNE USt-IdNr. zum allgemeinen Steuersatz (Aufwand)" 2 #f "94"))

   (cons 'U50 #(none "" "Leistungsempfänger als Steuerschuldner (§ 13b UStG)" 0 #f ""))
   (cons 'K46 #(none "46" "Im Inland steuerpflichtige sonstige Leistungen aus EU-Ländern (§13b Abs. 1 UStG) (Aufwand)" 2 #f "46"))
   (cons 'K52 #(none "52" "Andere Leistungen eines im Ausland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 1 und 5 UStG) (Aufwand)" 2 #f "52"))
   (cons 'K73 #(none "73" "Lieferungen sicherheitsübereigneter Gegenstände und Umsätze die unter das GrEStG fallen (Aufwand)" 2 #f "73"))
   (cons 'K84 #(none "84" "Andere Umsätze eines im Inland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 4 und Nr. 6 bis 9 UStG) (Aufwand)" 2 #f "84"))

   (cons 'U60 #(none "" "Abziehbare Vorsteuerbeträge" 0 #f ""))
   (cons 'K66 #(none "66" "Vorsteuerbeträge aus Rechnungen von anderen Unternehmern (§ 15 Abs. 1 Satz 1 Nr. 1 UStG)" 1 #f "66"))
   (cons 'K61 #(none "61" "Vorsteuerbeträge aus dem innergemeinschaftlichen Erwerb von Gegenständen (Gebuchte VSt)" 1 #f "61"))
   (cons 'K62 #(none "62" "Entrichtete Einfuhrumsatzsteuer (§ 15 Abs. 1 Satz 1 Nr. 2 UStG)" 1 #f "62"))
   (cons 'K67 #(none "67" "Vorsteuerbeträge aus Leistungen im Sinne des § 13b UStG (§ 15 Abs. 1 Satz 1 Nr. 4 UStG) (Gebuchte VSt)" 1 #f "67"))
   (cons 'K63 #(none "63" "Vorsteuerbeträge, die nach allgemeinen Durchschnittssätzen berechnet sind (§§ 23 und 23a UStG)" 1 #f "63"))
   (cons 'K64 #(none "64" "Berichtigung des Vorsteuerabzugs (§ 15a UStG)" 1 #f "64"))
   (cons 'K59 #(none "59" "Vorsteuerabzug für innergemeinschaftl. Lieferungen neuer Fahrzeuge außerhalb eines Unternehmens" 1 #f "59"))

   (cons 'U70 #(none "" "Andere Steuerbeträge" 0 #f ""))
   (cons 'K39 #(none "39" "Anrechnung (Abzug) der festgesetzten Sondervorauszahlung (1/11) für Dauerfristverlängerung" 1 #f "39"))

   )
  )
  (cons 'Other
   (list
   (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))
   )
  )

   ))

(define txf-asset-categories
 (list
  (cons 'Ind
   (list
   (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))
   (cons 'UST #(none "" "Diese und nachfolgende Kategorien ohne Nummer --> NICHT VERWENDEN !" 0 #f ""))

   (cons 'U30 #(none "" "Innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'U31 #(none "" "Steuerfreie innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'K91 #(none "91" "Erwerbe nach § 4b UStG" 2 #f "91"))
   (cons 'U32 #(none "" "Steuerpflichtige innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'K89 #(none "89" "zum Steuersatz von 19 %" 2 #f "89"))
   (cons 'K93 #(none "93" "zum Steuersatz von  7 %" 2 #f "93"))
   (cons 'K95 #(none "95" "zu anderen Steuersätzen" 2 #f "95"))
   (cons 'K94 #(none "94" "neuer Fahrzeuge von Lieferern OHNE USt-IdNr. zum allgemeinen Steuersatz (Aufwand)" 2 #f "94"))

   (cons 'U50 #(none "" "Leistungsempfänger als Steuerschuldner (§ 13b UStG)" 0 #f ""))
   (cons 'K46 #(none "46" "Im Inland steuerpflichtige sonstige Leistungen aus EU-Ländern (§13b Abs. 1 UStG) (Aufwand)" 2 #f "46"))
   (cons 'K52 #(none "52" "Andere Leistungen eines im Ausland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 1 und 5 UStG) (Aufwand)" 2 #f "52"))
   (cons 'K73 #(none "73" "Lieferungen sicherheitsübereigneter Gegenstände und Umsätze die unter das GrEStG fallen (Aufwand)" 2 #f "73"))
   (cons 'K84 #(none "84" "Andere Umsätze eines im Inland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 4 und Nr. 6 bis 9 UStG) (Aufwand)" 2 #f "84"))

   (cons 'U60 #(none "" "Abziehbare Vorsteuerbeträge" 0 #f ""))
   (cons 'K66 #(none "66" "Vorsteuerbeträge aus Rechnungen von anderen Unternehmern (§ 15 Abs. 1 Satz 1 Nr. 1 UStG)" 1 #f "66"))
   (cons 'K61 #(none "61" "Vorsteuerbeträge aus dem innergemeinschaftlichen Erwerb von Gegenständen (Gebuchte VSt)" 1 #f "61"))
   (cons 'K62 #(none "62" "Entrichtete Einfuhrumsatzsteuer (§ 15 Abs. 1 Satz 1 Nr. 2 UStG)" 1 #f "62"))
   (cons 'K67 #(none "67" "Vorsteuerbeträge aus Leistungen im Sinne des § 13b UStG (§ 15 Abs. 1 Satz 1 Nr. 4 UStG) (Gebuchte VSt)" 1 #f "67"))
   (cons 'K63 #(none "63" "Vorsteuerbeträge, die nach allgemeinen Durchschnittssätzen berechnet sind (§§ 23 und 23a UStG)" 1 #f "63"))
   (cons 'K64 #(none "64" "Berichtigung des Vorsteuerabzugs (§ 15a UStG)" 1 #f "64"))
   (cons 'K59 #(none "59" "Vorsteuerabzug für innergemeinschaftl. Lieferungen neuer Fahrzeuge außerhalb eines Unternehmens" 1 #f "59"))

   )
  )
  (cons 'Other
   (list
   (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))
   )
  )
))

(define txf-liab-eq-categories
 (list
  (cons 'Ind
   (list
   (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))
   (cons 'UST #(none "" "Diese und nachfolgende Kategorien ohne Nummer --> NICHT VERWENDEN !" 0 #f ""))

   (cons 'U20 #(none "" "Steuerpflichtige Umsätze (Lieferungen u. sonst. Leistungen einschl. unentgeltlicher Wertabgaben)" 0 #f ""))
   (cons 'K36 #(none "36" "zu anderen Steuersätzen (MWSt)" 1 #f "36"))
   (cons 'U21 #(none "" "Lieferungen land- und forstwirtschaftlicher Betriebe nach § 24 UStG" 0 #f ""))
   (cons 'K80 #(none "80" "Umsätze, für die eine Steuer nach § 24 UStG zu entrichten ist (MWSt)" 1 #f "80"))

   (cons 'U30 #(none "" "Innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'U32 #(none "" "Steuerpflichtige innergemeinschaftliche Erwerbe" 0 #f ""))
   (cons 'K98 #(none "98" "zu anderen Steuersätzen (Gebuchte MWSt)" 1 #f "98"))
   (cons 'K96 #(none "96" "neuer Fahrzeuge von Lieferern OHNE USt-IdNr. zum allgemeinen Steuersatz (Gebuchte MWSt)" 1 #f "96"))

   (cons 'U50 #(none "" "Leistungsempfänger als Steuerschuldner (§ 13b UStG)" 0 #f ""))
   (cons 'K47 #(none "47" "Im Inland steuerpflichtige sonstige Leistungen aus EU-Ländern (§13b Abs. 1 UStG) (Gebuchte MWSt)" 1 #f "47"))
   (cons 'K53 #(none "53" "Andere Leistungen eines im Ausland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 1 und 5 UStG) (Gebuchte MWSt)" 1 #f "53"))
   (cons 'K74 #(none "74" "Lieferungen sicherheitsübereigneter Gegenstände und Umsätze, die unter das GrEStG fallen (Gebuchte MWSt)" 1 #f "74"))
   (cons 'K85 #(none "85" "Andere Umsätze eines im Inland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 4 und Nr. 6 bis 9 UStG) (Gebuchte MWSt)" 1 #f "85"))

   (cons 'U70 #(none "" "Andere Steuerbeträge" 0 #f ""))
   (cons 'K65 #(none "65" "Steuer infolge Wechsels der Besteuerungsform sowie Nachsteuer auf versteuerte Anzahlungen u. ä." 1 #f "65"))
   (cons 'K69 #(none "69" "In Rechnungen unrichtig oder unberechtigt ausgewiesene Steuerbeträge (§ 14c UStG)" 1 #f "69"))
   (cons 'K39 #(none "39" "Anrechnung (Abzug) der festgesetzten Sondervorauszahlung (1/11) für Dauerfristverlängerung" 1 #f "39"))

   )
  )
  (cons 'Other
   (list
   (cons 'N000 #(none "" "Informativ im Steuerbericht anzeigen -- Kein Export -- " 0 #f ""))
   )
  )
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
