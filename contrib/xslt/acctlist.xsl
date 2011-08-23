<xsl:stylesheet version="1.0" 
		xmlns="http://www.gnucash.org/XML/"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:gnc="http://www.gnucash.org/XML/gnc"
		xmlns:act="http://www.gnucash.org/XML/act">
  <xsl:output method="text" encoding="utf-8"/>
  <xsl:strip-space elements="*"/>

  <xsl:param name="separator">,</xsl:param>

  <xsl:param name="quote">"</xsl:param>

  <xsl:param name="newline">&#10;</xsl:param>

  <xsl:template match="/">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="gnc-v2|gnc:book|gnc-account-example">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="gnc:account">
    <xsl:apply-templates select="act:code"/>
    <xsl:value-of select="$separator"/>
    <xsl:apply-templates select="act:name"/>
    <xsl:value-of select="$separator"/>
    <xsl:apply-templates select="act:type"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="act:code|act:name|act:type">
    <xsl:value-of select="$quote"/>
    <xsl:value-of select="."/>
    <xsl:value-of select="$quote"/>
  </xsl:template>

  <xsl:template match="*"/>

</xsl:stylesheet>
  
