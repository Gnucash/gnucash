# Happy New Year mit GnuCash! #

Dieses Projekt hilft beim Wechsel in ein neues Finanzjahr mit GnuCash.

Es gibt die folgenden Übersetzungen für diese Dokumentation:

- 🇬🇧 Englisch: [README.md](README.md)
- 🇩🇪 Deutsch / German: [REAMDE.de.md](README.de.md) (das liest du gerade)

Für eine saubere Archivierung und aus rechtlichen Gründen kann es notwendig sein, die GnuCash-Datei
des letzten Jahres "einzufrieren" und eine neue Datei mit identischer Kontenstruktur und Salden zu
erstellen, jedoch ohne alle Buchungen des vergangenen Jahres.  Das ist genau das, was das Programm
in diesem Projekt macht:

- Erstellen einer neuen GnuCash-Datei.
- Duplizieren der Originalkonten.
- Erstellen einer oder mehrerer Eröffnungsbuchungen, um die Konten mit dem richtigen Wert zu
  initialisieren.
- Duplizieren von Business-Objekten wie Lieferanten, Kunden, Mitarbeiter.

## Erste Schritte ##

### Voraussetzungen ###

Stelle sicher, dass du GnuCash und seine Python-Bibliotheken installiert hast, sowie das
sqlite-Backend (`libdbd-sqlite3` auf Debian).  Das kannst du überprüfen, indem du folgenden Befehl
aufrufst:  
`python3 -c "import gnucash"`.  Wenn es dabei keine Fehlermeldungen gibt, sollte alles
funktionieren.

### Das Programm ausführen ###

- Um eine Liste der Optionen zu erhalten, rufe `./new_year.py -h` auf.
- Um eine neue Datei aus einer vorhandenen zu erstellen, benutze die Optionen `-i` und `-o`:  
  `./new_year.py -i examples/lastyear.gnucash -o new.gnucash`
  - Damit wird die GnuCash-Datei `examples/lastyear.gnucash` gelesen und aus ihrem Inhalt eine neue
    Datei `new.gnucash` erstellt.
  - Du kannst nun `new.gnucash` mit dem GnuCash-Programm öffnen, um die Ergebnisse anzuschauen.
  - Wenn `new.gnucash` bereits existiert, wird eine Warnung ausgegeben und die Buchungen werden
    trotzdem auf die Ausgabedatei angewendet, möglicherweise werden dabei mehr Daten hinzugefügt als
    du willst!

## Weitere Optionen: Zielkonten und Konfiguration ##

Es kann wünschenswert sein, die Eröffnungstransaktion in mehrere Teile aufzuteilen, z. B. um
getrennte Buchungen für Aktiva und Passiva zu haben.  Zu diesem Zweck kannst du *Zielkonten* für
bestimmte Arten von Konten definieren.  Eine Möglichkeit für diese Definitionen sind die
Befehlszeilenoptionen `--target-asset`, `--target-liability` usw., denen Kontonamen für die
Eröffnungsbuchungen eines bestimmten Typs zugeordnet werden (getrennt durch Doppelpunkte "`:`").
Alternativ können Benutzer:innen diese Definitionen auch in eine Konfigurationsdatei schreiben und
diese mit der Option `--conf` angeben.

Die Konfigurationsdatei verwendet das INI-Format und alle Konfigurationen befinden sich im Abschnitt
`[DEFAULT]`.

Die Parameterwerte werden als Key-Value-Paare gespeichert, die Schlüssel sind identisch mit den
Kommandozeilenoptionen, wobei die Bindestriche `-` durch Unterstriche `_` ersetzt werden.

Eine Beispielkonfigurationsdatei befindet sich im Verzeichnis `test_data/`.

## Bei der Weiterentwicklung mitmachen ##

Möchtest du bei der Entwicklung helfen? Das ist großartig!  Hier sind eine Reihe von Möglichkeiten,
wie du dazu beitragen kannst, diese Software noch besser zu machen:

- Nutze die Software, finde Probleme und [erstelle ein Issue](https://gitlab.com/wiese28/gnucash-happy-new-year/-/issues).
- Auch wenn du eine Idee hast, wie die Software verbessert werden könnte, kannst du gerne ein Ticket
  eröffnen.
- Verbessere diese Dokumentation oder schreibe Übersetzungen.
- Behebe selbst Fehler oder implementiere neue Funktionen.  Bitte öffne dafür einen Merge Request,
  damit deine Änderungen hier aufgenommen werden.
- Schreibe Tests (siehe unten) für weitere Teile des Codes.
- Hilf beim Refactoring des Codes, damit er pythonischer wird und ein richtiges Python-Paket wird.
  Das kann auch Tools beinhalten, die den Code von GnuCash einfacher zu benutzen machen.

### Testen ###

Im Moment bestehen die Tests aus wenig mehr als einem Proof of Concept.  Das sollte verbessert
werden.

Um die Tests auszuführen, muss pytest installiert und aufgerufen werden: `pytest tests`

Wenn du neue Tests schreiben willst, kannst du `import utils` benutzen, um `sys.path` passend zu
setzen.
