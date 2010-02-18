;; -*-scheme-*-
;;
;; This file was copied from the file txf.scm by  Richard -Gilligan- Uschold
;;
;; Originally, these were meant to hold the codes for the US tax TXF
;; format. Christian Stimming modified this heavily so that it might become useful for
;; the German Umsatzsteuer-Voranmeldung. 
;; Further modifications by:
;;   Jannick Asmus
;;   Frank H. Ellenberger
;;   Rolf Leggewie
;;
;; This file holds the explanations to the categories from txf-de_DE.scm.
;;

(define txf-help-strings
  '(
    (H001 . "Bei mit \"&lt;\" oder \"^\" markierten Kategorien wird neben dem Betrag eine Beschreibung exportiert.  \"&lt;\" indiziert, daß der Name von diesem Konto als Beschreibung exportiert wird.  Genaueres kann dem jeweiligen Formular entnommen werden.")
    (H002 . "Bei mit \"&lt;\" oder \"^\" markierten Kategorien wird neben dem Betrag eine Beschreibung exportiert.  \"^\" indiziert, daß der Name des übergeordneten Kontos als Beschreibung exportiert wird.  Genaueres kann dem jeweiligen Formular entnommen werden.")
    (H003 . "Mit \"#\" markierte Kategorien sind noch nicht vollständig implementiert!  Bitte diese Codes nicht verwenden!")
    (N000 . "Dies ist eine Pseudokategorie, welche zwar im Bericht angezeigt, aber nicht exportiert wird.")

    (K41 . "Innergemeinschaftliche Lieferungen (§ 4 Nr. 1 Buchst. b UStG) an Abnehmer mit USt-IdNr.  (Bemessungsgrundlage)")
    (K44 . "Innergemeinschaftliche Lieferungen neuer Fahrzeuge an Abnehmer ohne USt-IdNr (Bemessungsgrundlage)")
    (K49 . "Innergemeinschaftliche Lieferungen neuer Fahrzeuge außerhalb eines Unternehmens (§ 2a UStG)  (Bemessungsgrundlage)")
    (K43 . "Weitere steuerfreie Umsätze mit Vorsteuerabzug (z.B. Ausfuhrlieferungen, Umsätze nach § 4 Nr. 2 bis 7 UStG) (Bemessungsgrundlage)")
    (K48 . "Steuerfreie Umsätze ohne Vorsteuerabzug: Umsätze nach § 4 Nr. 8 bis 28 UStG (Bemessungsgrundlage)")

    (K51 . "Steuerpflichtige Umsätze (Lieferungen und sonstige Leistungen einschl. unentgeltlicher Wertabgaben) zum Steuersatz von 16 v.H. (Bemessungsgrundlage)")
    (K81 . "Steuerpflichtige Umsätze (Lieferungen und sonstige Leistungen einschl. unentgeltlicher Wertabgaben) zum Steuersatz von 19 v.H. (Bemessungsgrundlage)")
    (K86 . "Steuerpflichtige Umsätze (Lieferungen und sonstige Leistungen einschl. unentgeltlicher Wertabgaben) zum Steuersatz von 7 v.H. (Bemessungsgrundlage)")
    (K35 . "Umsätze, die anderen Steuersätzen unterliegen (Bemessungsgrundlage)")
    (K36 . "Umsätze, die anderen Steuersätzen unterliegen (Steuer)")
    (K77 . "Umsätze land- und forstwirtschaftlicher Betriebe nach § 24 UStG: Lieferungen in das übrige Gemeinschaftsgebiet an Abnehmer mit USt-IdNr. (Bemessungsgrundlage)")
    (K76 . "Umsätze, für die eine Steuer nach § 24 UStG zu entrichten ist (Sägewerkserzeugnisse, Getränke und alkohol. Flüssigkeiten, z.B. Wein) (Bemessungsgrundlage)")
    (K80 . "Umsätze, für die eine Steuer nach § 24 UStG zu entrichten ist (Sägewerkserzeugnisse, Getränke und alkohol. Flüssigkeiten, z.B. Wein) (Steuer)")

    (K89 . "Steuerpflichtige innergemeinschaftliche Erwerbe zum Steuersatz von 19 v.H. (Bemessungsgrundlage)")
    (K91 . "Steuerfreie innergemeinschaftliche Erwerbe: Erwerbe nach § 4b UStG (Bemessungsgrundlage)")
    (K93 . "Steuerpflichtige innergemeinschaftliche Erwerbe zum Steuersatz von 7 v.H. (Bemessungsgrundlage)")

    (K66 . "Vorsteuerbeträge aus Rechnungen von anderen Unternehmern (§ 15 Abs. 1 Satz 1 Nr. 1 UStG), aus Leistungen im Sinne des § 13a Abs. 1 Nr. 6 UStG (§ 15 Abs. 1 Satz 1 Nr. 5 UStG) und aus innergemeinschaftlichen Dreiecksgeschäften (§ 25b Abs. 5 UStG)")
    ))
