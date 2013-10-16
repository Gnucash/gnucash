<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:df="http://dateformats.data">
<xsl:output method="text" encoding="UTF8"/>

<!-- Configure lookup table for date format -->
<xsl:key name="datefmt-lookup" match="df:dateformat" use="df:name"/>
<xsl:variable name="dateformats-top" select="document('')/*/df:dateformats"/>

<!-- Primary template - process each prefence group -->
<xsl:template match="/">
<!-- Write file header -->
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  migrate-prefs.scm
;;;  Custom generated script to migrate user preferences from
;;;  gconf to gsettings. This should only be run once -
;;;  when running GnuCash 2.6.x for the first time.
;;;
;;;  Copyright 2013 Geert Janssens &lt;geert@kobaltwit.be&gt;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (migrate-prefs-user))

(use-modules (gnucash core-utils))
;(use-modules (gnucash gnc-module))
;; Guile 2 needs to find the symbols from the c module at compile time already
;(cond-expand
;  (guile-2
;    (eval-when
;      (compile load eval) 
;      (load-extension "libgnc-core-utils" "scm_init_sw_core_utils_module")))
;  (else
;    (load-extension "libgnc-core-utils" "scm_init_sw_core_utils_module")))
;(use-modules (sw_core_utils))

(define (run-migration-internal)
 <xsl:for-each select="//prefsgroup">
  <xsl:if test="document(gconfpath)//entry">
;; Processing preferences in group <xsl:value-of select="gschemaid"/>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   <xsl:variable name="gconf-top" select="document(gconfpath)/gconf"/>
   <xsl:apply-templates select="./pref"/>
  </xsl:if>
 </xsl:for-each>
 
(display "Preference migration has finished")(newline)
)

(define (run-migration)
  (catch #t
    run-migration-internal
    (lambda args 
        (display (string-append
                   "An error occurred while migrating preferences."
                   (newline) "The error is: "
                   (symbol->string key) " - "  (car (caddr args))  "."))
        #f))
)

(export run-migration)
</xsl:template>

<!-- Process one prefence -->
<xsl:template match="pref">
  <xsl:variable name="gconf-top" select="document(../gconfpath)/gconf"/>
  <xsl:apply-templates select="$gconf-top">
   <xsl:with-param name="curr-pref" select="."/>
  </xsl:apply-templates>
</xsl:template>
 
<!-- Find the equivalent entry in gconf -->
<xsl:template match="gconf">
 <xsl:param name="curr-pref"/>
 <xsl:for-each select="entry">
  <xsl:if test="@name = $curr-pref/gconfkey">
;; Processing preference <xsl:value-of select="$curr-pref/gschemaname"/>
   <xsl:apply-templates select=".">
    <xsl:with-param name="curr-pref" select="$curr-pref"/>
   </xsl:apply-templates>
  </xsl:if>
 </xsl:for-each>
</xsl:template>

<!-- Determine next action based on preference type -->
<xsl:template match="entry">
 <xsl:param name="curr-pref"/>
  <xsl:choose>
   <xsl:when test="$curr-pref/gschematype = 'b'"><xsl:if test="./@value">
;; Gconf value (boolean): <xsl:value-of select="./@value"/>
(gnc-prefs-set-bool
    ; preference group
    "<xsl:value-of select="$curr-pref/../gschemaid"/>"
    ; preference name
    "<xsl:value-of select="$curr-pref/gschemaname"/>"
    ; preference value
    <xsl:if test="./@value = 'true'">#t</xsl:if>
    <xsl:if test="./@value = 'false'">#f</xsl:if>
)
   </xsl:if></xsl:when>
   

   <xsl:when test="$curr-pref/gschematype = 'datefmt'"><xsl:if test="./stringvalue">
;; Gconf value (string): "<xsl:value-of select="./stringvalue"/>" -> gsettings (integer)
(gnc-prefs-set-int
    ; preference group
    "<xsl:value-of select="$curr-pref/../gschemaid"/>"
    ; preference name
    "<xsl:value-of select="$curr-pref/gschemaname"/>"
    ; preference value
    <xsl:apply-templates select="$dateformats-top">
     <xsl:with-param name="curr-entry" select="."/>
    </xsl:apply-templates>
)
   </xsl:if></xsl:when>


   <xsl:when test="$curr-pref/gschematype = '(dd)'">
;; Type: pair of decimals (stored in Gconf as list of floats)
(let ((coords '()))
     <xsl:for-each select="./li">
     (set! coords (append coords '(<xsl:value-of select="./@value"/>)))
     </xsl:for-each>
     (if (> (length coords) 1)
         (gnc-prefs-set-coords
             ; preference group
             "<xsl:value-of select="$curr-pref/../gschemaid"/>"
             ; preference name
             "<xsl:value-of select="$curr-pref/gschemaname"/>"
             ; x coord
             (car coords)
             ; y coord
             (cadr coords))))
   </xsl:when>


   <xsl:when test="$curr-pref/gschematype = 'd'"><xsl:if test="./@value">
;; Gconf value (decimal): <xsl:value-of select="./@value"/>
(gnc-prefs-set-float
    ; preference group
    "<xsl:value-of select="$curr-pref/../gschemaid"/>"
    ; preference name
    "<xsl:value-of select="$curr-pref/gschemaname"/>"
    ; preference value
    <xsl:value-of select="./@value"/>
)
   </xsl:if></xsl:when>


   <xsl:when test="$curr-pref/gschematype = 'i'"><xsl:if test="./@value">
;; Gconf value (integer): <xsl:value-of select="./@value"/>
(gnc-prefs-set-int
    ; preference group
    "<xsl:value-of select="$curr-pref/../gschemaid"/>"
    ; preference name
    "<xsl:value-of select="$curr-pref/gschemaname"/>"
    ; preference value
    <xsl:value-of select="./@value"/>
)
   </xsl:if></xsl:when>


   <xsl:when test="$curr-pref/gschematype = 's2b'"><xsl:if test="./stringvalue">
;; Gconf value (string): "<xsl:value-of select="./stringvalue"/>" -> gsettings (boolean)
(let ((suffix (string-delete "<xsl:value-of select="./stringvalue"/>" #\_)))
     (gnc-prefs-set-bool
         ; preference group
         "<xsl:value-of select="$curr-pref/../gschemaid"/>"
         ; preference name
         "<xsl:value-of select="$curr-pref/gschemaname"/>"
         ; preference value
         (string-suffix? (string-append "-" suffix) "<xsl:value-of select="$curr-pref/gschemaname"/>")))
   </xsl:if></xsl:when>


   <xsl:when test="$curr-pref/gschematype = 's'"><xsl:if test="./stringvalue">
;; Gconf value (string): "<xsl:value-of select="./stringvalue"/>"
(gnc-prefs-set-string
    ; preference group
    "<xsl:value-of select="$curr-pref/../gschemaid"/>"
    ; preference name
    "<xsl:value-of select="$curr-pref/gschemaname"/>"
    ; preference value
    "<xsl:value-of select="translate(./stringvalue, $win, $mingw)"/>"
)
   </xsl:if></xsl:when>


   <xsl:when test="$curr-pref/gschematype = 'x'"><xsl:if test="./@value">
;; Gconf value (64bit integer): <xsl:value-of select="./@value"/>
(gnc-prefs-set-int64
    ; preference group
    "<xsl:value-of select="$curr-pref/../gschemaid"/>"
    ; preference name
    "<xsl:value-of select="$curr-pref/gschemaname"/>"
    ; preference value
    <xsl:value-of select="./@value"/>
)
   </xsl:if></xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template match="df:dateformats">
 <xsl:param name="curr-entry"/>
 <xsl:value-of select="key('datefmt-lookup', $curr-entry/stringvalue)/df:index"/>
</xsl:template>

<xsl:variable name="win" select="'\'" />
<xsl:variable name="mingw" select="'/'" />


<df:dateformats>
  <df:dateformat><df:name>us</df:name><df:index>0</df:index></df:dateformat>
  <df:dateformat><df:name>uk</df:name><df:index>1</df:index></df:dateformat>
  <df:dateformat><df:name>ce</df:name><df:index>2</df:index></df:dateformat>
  <df:dateformat><df:name>iso</df:name><df:index>3</df:index></df:dateformat>
  <df:dateformat><df:name>locale</df:name><df:index>4</df:index></df:dateformat>
</df:dateformats>

</xsl:stylesheet>