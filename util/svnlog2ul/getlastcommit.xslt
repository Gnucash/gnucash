<?xml version="1.0" encoding="utf-8"?>

<!DOCTYPE xsl:stylesheet [
 <!ENTITY tab "&#9;">
 <!ENTITY newl "&#10;">
 <!ENTITY space "&#32;">
]>

<xsl:stylesheet
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  >

 <xsl:output
   method="text"
   encoding="utf-8"
   media-type="text/plain"
   omit-xml-declaration="yes"
   standalone="yes"
   indent="no" />

  <xsl:template match="/">
    <xsl:value-of select="info/entry/commit/attribute::revision" />
  </xsl:template>
</xsl:stylesheet>