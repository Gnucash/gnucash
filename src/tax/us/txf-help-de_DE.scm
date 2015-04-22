;; -*-scheme-*-
;;
;; This file was copied from the file txf.scm by  Richard -Gilligan- Uschold
;;
;; Originally, these were meant to hold the codes for the US tax TXF
;; format. Christian Stimming modified this heavily so that it might become useful for
;; the German Umsatzsteuer-Voranmeldung. 
;;
;; Diese Datei wurde im Dezember 2010 für GnuCash Vers. 2.4.0 vollständig überarbeitet und alle Einträge
;; gemäß der "Umsatzsteuer-Voranmeldung 2011" umfassend berichtigt und komplettiert von FJSW - Franz Stoll
;;
;;
;; This file holds the explanations to the categories from txf-de_DE.scm.
;;
;; Changes in this file should also be applied on gnucash-docs/help/de/Help_txf-categories.xml

(define txf-help-strings
  '(
    (H001 . "Bei mit \"&lt;\" oder \"^\" markierten Kategorien wird neben dem Betrag eine Beschreibung exportiert.  \"&lt;\" indiziert, daß der Name von diesem Konto als Beschreibung exportiert wird.  Genaueres kann dem jeweiligen Formular entnommen werden.")
    (H002 . "Bei mit \"&lt;\" oder \"^\" markierten Kategorien wird neben dem Betrag eine Beschreibung exportiert.  \"^\" indiziert, daß der Name des übergeordneten Kontos als Beschreibung exportiert wird.  Genaueres kann dem jeweiligen Formular entnommen werden.")
    (H003 . "Mit \"#\" markierte Kategorien sind noch nicht vollständig implementiert!  Bitte diese Codes nicht verwenden!")
    (N000 . "Bei Auswahl dieser Kategorie wird das Konto im Steuerbericht zur Information angezeigt, aber nicht exportiert.")
    (UST . "Diese und nachfolgende Kategorien ohne Nummer ==>> N I C H T   V E R W E N D E N ! <<==
Diese nummernlose Kategorien führen bei einer Verwendung zu FEHLERN beim XML-Export, da sie einer leeren UStVA-Kennzahl zugeordnet würden.")


    (UST . "Lieferungen und sonstige Leistungen (einschließlich unentgeltlicher Wertabgaben)")
    (UST . "Steuerfreie Umsätze mit Vorsteuerabzug")
    (UST . "Innergemeinschaftliche Lieferungen (§ 4 Nr. 1 Buchst. b UStG)")
    (K41 . "Innergemeinschaftliche Lieferungen (§ 4 Nr. 1 Buchst. b UStG) an Abnehmer MIT USt-IdNr.
(Erlöse)")
    (K44 . "Innergemeinschaftliche Lieferungen (§ 4 Nr. 1 Buchst. b UStG) neuer Fahrzeuge an Abnehmer OHNE USt-IdNr.
(Erlöse)")
    (K49 . "Innergemeinschaftliche Lieferungen (§ 4 Nr. 1 Buchst. b UStG) neuer Fahrzeuge außerhalb eines Unternehmens (§ 2a UStG)
(Erlöse)")
    (UST . "Weitere steuerfreie Umsätze mit Vorsteuerabzug")
    (K43 . "Weitere steuerfreie Umsätze mit Vorsteuerabzug (z.B. Ausfuhrlieferungen, Umsätze nach § 4 Nr. 2 bis 7 UStG)
(Erlöse)")
    (UST . "Steuerfreie Umsätze ohne Vorsteuerabzug")
    (K48 . "Steuerfreie Umsätze ohne Vorsteuerabzug Umsätze nach § 4 Nr. 8 bis 28 UStG
(Erlöse)")

    (UST . "Steuerpflichtige Umsätze (Lieferungen und sonstige Leistungen einschl. unentgeltlicher Wertabgaben)")
    (K51 . "Steuerpflichtige Umsätze zum Steuersatz von 16 % (bis incl. 2006)
(Erlöse)")
    (K81 . "Steuerpflichtige Umsätze zum Steuersatz von 19 %
(Erlöse)")
    (K86 . "Steuerpflichtige Umsätze zum Steuersatz von  7 %
(Erlöse)")
    (K35 . "Steuerpflichtige Umsätze zu anderen Steuersätzen
(Erlöse)")
    (K36 . "Steuerpflichtige Umsätze zu anderen Steuersätzen
(Mehrwertsteuer)")
    (UST . "Lieferungen land- und forstwirtschaftlicher Betriebe nach § 24 UStG")
    (K77 . "Lieferungen land- und forstwirtschaftlicher Betriebe nach § 24 UStG an Abnehmer MIT USt-IdNr.
(Erlöse)")
    (K76 . "Lieferungen land- und forstwirtschaftlicher Betriebe, für die eine Steuer nach § 24 UStG zu entrichten ist (Sägewerkserzeugnisse, Getränke und alkohol. Flüssigkeiten, z.B. Wein)
(Erlöse)")
    (K80 . "Lieferungen land- und forstwirtschaftlicher Betriebe, für die eine Steuer nach § 24 UStG zu entrichten ist (Sägewerkserzeugnisse, Getränke und alkohol. Flüssigkeiten, z.B. Wein)
(Mehrwertsteuer)")

    (UST . "Innergemeinschaftliche Erwerbe")
    (UST . "Steuerfreie innergemeinschaftliche Erwerbe")
    (K91 . "Steuerfreie innergemeinschaftliche Erwerbe nach § 4b UStG
(Aufwand)")
    (UST . "Steuerpflichtige innergemeinschaftliche Erwerbe")
    (K89 . "Steuerpflichtige innergemeinschaftliche Erwerbe zum Steuersatz von 19 %
(Aufwand)")
    (K93 . "Steuerpflichtige innergemeinschaftliche Erwerbe zum Steuersatz von  7 %
(Aufwand)")
    (K95 . "Steuerpflichtige innergemeinschaftliche Erwerbe zu anderen Steuersätzen
(Aufwand)")
    (K98 . "Steuerpflichtige innergemeinschaftliche Erwerbe zu anderen Steuersätzen
(Gebuchte Mehrwertsteuer)")
    (K94 . "Steuerpflichtige innergemeinschaftliche Erwerbe neuer Fahrzeuge von Lieferern OHNE USt-IdNr. zum allgemeinen Steuersatz
(Aufwand)")
    (K96 . "Steuerpflichtige innergemeinschaftliche Erwerbe neuer Fahrzeuge von Lieferern OHNE USt-IdNr. zum allgemeinen Steuersatz
(Gebuchte Mehrwertsteuer)")

    (UST . "Ergänzende Angaben zu Umsätzen")
    (K42 . "Lieferungen des ersten Abnehmers bei innergemeinschaftlichen Dreiecksgeschäften (§ 25b Abs. 2 UStG)
(Erlöse)")
    (K60 . "Steuerpflichtige Umsätze, für die der Leistungsempfänger die Steuer nach § 13b Abs. 5 UStG schuldet
(Erlöse)")
    (K21 . "Nicht steuerbare sonstige Leistungen gem. § 18b Satz 1 Nr. 2 UStG (Leistungen EU-Land)
(Erlöse)")
    (K45 . "Übrige nicht steuerbare Umsätze (Leistungsort nicht im Inland)
(Erlöse)")

    (UST . "Leistungsempfänger als Steuerschuldner (§ 13b UStG)")
    (K46 . "Im Inland steuerpflichtige sonstige Leistungen von im übrigen Gemeinschaftsgebiet ansässigen Unternehmern (§13b Abs. 1 UStG)
(Aufwand)")
    (K47 . "Im Inland steuerpflichtige sonstige Leistungen von im übrigen Gemeinschaftsgebiet ansässigen Unternehmern (§13b Abs. 1 UStG)
(Gebuchte Mehrwertsteuer)")
    (K52 . "Andere Leistungen eines im Ausland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 1 und 5 UStG)
(Aufwand)")
    (K53 . "Andere Leistungen eines im Ausland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 1 und 5 UStG)
(Gebuchte Mehrwertsteuer)")
    (K73 . "Lieferungen sicherheitsübereigneter Gegenstände und Umsätze, die unter das GrEStG fallen (§ 13b Abs. 2 Nr. 2 und 3 UStG)
(Aufwand)")
    (K74 . "Lieferungen sicherheitsübereigneter Gegenstände und Umsätze, die unter das GrEStG fallen (§ 13b Abs. 2 Nr. 2 und 3 UStG)
(Gebuchte Mehrwertsteuer)")
    (K84 . "Andere Umsätze eines im Inland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 4 und Nr. 6 bis 9 UStG)
(Aufwand)")
    (K85 . "Andere Umsätze eines im Inland ansässigen Unternehmers (§ 13b Abs. 2 Nr. 4 und Nr. 6 bis 9 UStG)
(Gebuchte Mehrwertsteuer)")
    (K65 . "Steuer infolge Wechsels der Besteuerungsform sowie Nachsteuer auf versteuerte Anzahlungen u. ä. wegen Steuersatzänderung
(Gebuchte Mehrwertsteuer --> Mehr- oder ggf. auch Minderbetrag)")

    (UST . "Abziehbare Vorsteuerbeträge")
    (K66 . "Vorsteuerbeträge aus Rechnungen von anderen Unternehmern (§ 15 Abs. 1 Satz 1 Nr. 1 UStG), aus Leistungen im Sinne des § 13a Abs. 1 Nr. 6 UStG (§ 15 Abs. 1 Satz 1 Nr. 5 UStG) und aus innergemeinschaftlichen Dreiecksgeschäften (§ 25b Abs. 5 UStG)
(Vorsteuer allgemein)")
    (K61 . "Vorsteuerbeträge aus dem innergemeinschaftlichen Erwerb von Gegenständen (§ 15 Abs. 1 Satz 1 Nr. 3 UStG)
(Gebuchte Vorsteuer)")
    (K62 . "Entrichtete (abziehbare) Einfuhrumsatzsteuer (§ 15 Abs. 1 Satz 1 Nr. 2 UStG)
(bezahlte EUSt)")
    (K67 . "Vorsteuerbeträge aus Leistungen im Sinne des § 13b UStG (§ 15 Abs. 1 Satz 1 Nr. 4 UStG)
(Gebuchte Vorsteuer)")
    (K63 . "Vorsteuerbeträge, die nach allgemeinen Durchschnittssätzen berechnet sind (§§ 23 und 23a UStG)
(Gebuchte Vorsteuer)")
    (K64 . "Berichtigung des Vorsteuerabzugs - auf Wirtschaftsgüter - (§ 15a UStG)
(Gebuchte Vorsteuerberichtigungen --> Mehr- oder Minderbetrag)")
    (K59 . "Vorsteuerabzug für innergemeinschaftliche Lieferungen neuer Fahrzeuge außerhalb eines Unternehmens (§ 2a UStG) sowie von Kleinunternehmern im Sinne des § 19 Abs. 1 UStG (§ 15 Abs. 4a UStG)
(Gebuchte/bezahlte Vorsteuer)")

    (UST . "Andere Steuerbeträge")
    (K69 . "In Rechnungen unrichtig oder unberechtigt ausgewiesene Steuerbeträge (§ 14c UStG) sowie Steuerbeträge, die nach § 4 Nr. 4a Satz 1 Buchstabe a Satz 2, § 6a Abs. 4 Satz 2, § 17 Abs. 1 Satz 6 oder § 25b Abs. 2 UStG geschuldet werden
(Gebuchte Mehrwertsteuer --> Mehrbetrag)")
    (K39 . "Anrechnung (Abzug) der festgesetzten Sondervorauszahlung (1/11) für Dauerfristverlängerung
==> Das Konto »Umsatzsteuer-Vorauszahlung 1/11« darf nur für die letzte Voranmeldung des Besteuerungszeitraums, in der Regel Dezember, dieser Kennzahl zugeordnet werden, da sonst die Verrechnung vorzeitig bzw. gar nicht statt finden würde!")

    ))
