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
  (vector-ref (cdr (assv code categories)) index))

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

   (cons 'K41 #(none "41" "Innergemeinschaftliche Lieferungen an Abnehmer mit USt-IdNr. " 2 #f "41"))
   (cons 'K44 #(none "44" "Innergemeinschaftliche Lieferungen neuer Fahrzeuge an Abnehmer ohne USt-IdNr" 2 #f "44"))
   (cons 'K49 #(none "49" "Innergemeinschaftliche Lieferungen neuer Fahrzeuge außerhalb eines Unternehmens" 2 #f "49"))
   (cons 'K43 #(none "43" "Weitere steuerfreie Umsätze mit Vorsteuerabzug" 2 #f "43"))
   (cons 'K48 #(none "48" "Steuerfreie Umsätze ohne Vorsteuerabzug" 2 #f "48"))
   
   (cons 'K51 #(none "51" "Steuerpflichtige Umsätze, Steuersatz 16 v.H." 2 #f "51"))
   (cons 'K86 #(none "86" "Steuerpflichtige Umsätze, Steuersatz 7 v.H." 2 #f "86"))
   (cons 'K35 #(none "35" "Umsätze, die anderen Steuersätzen unterliegen (Bemessungsgrundlage)" 2 #f "35"))
   (cons 'K36 #(none "36" "Umsätze, die anderen Steuersätzen unterliegen (Steuer)" 1 #f "36"))
   (cons 'K77 #(none "77" "Umsätze land- und forstwirtschaftlicher Betriebe:: Lieferungen in das übrige Gemeinschaftsgebiet an Abnehmer mit USt-IdNr." 2 #f "77"))
   (cons 'K76 #(none "76" "Umsätze, für die eine Steuer nach § 24 UStG zu entrichten ist (Bemessungsgrundlage)" 2 #f "76"))
   (cons 'K80 #(none "80" "Umsätze, für die eine Steuer nach § 24 UStG zu entrichten ist (Steuer)" 1 #f "80"))
   ))


(define txf-expense-categories
  (list
   (cons 'N000 #(none "" "Nur zur Voransicht im Steuer-Bericht -- kein Export" 0 #f ""))

   (cons 'K91 #(none "91" "Steuerfreie innergemeinschaftliche Erwerbe" 2 #f "91"))

   (cons 'K66 #(none "66" "Vorsteuerbeträge aus Rechnungen von anderen Unternehmern" 1 #f "66"))
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

(gnc:register-kvp-option-generator gnc:id-book book-options-generator)
