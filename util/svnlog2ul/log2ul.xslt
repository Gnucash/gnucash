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
  <xsl:output method="xml"
              doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN"
              doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"
              indent="yes"/>

  <xsl:template match="/">
<html>
<head>
</head>
<body>
  <ul>
    <xsl:apply-templates/>
  </ul>
</body>
</html>
  </xsl:template>

  <xsl:template match="logentry">
<li>
<xsl:value-of select="msg" />
</li>
  </xsl:template>
</xsl:stylesheet>
