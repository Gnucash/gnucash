<?xml version="1.0" encoding="utf-8"?>
<!--
 acctlist.xsl: an XSLT style sheet to transform an account
 hierarchy into a comma-separated list of account numbers,
 names, and types.

 Copyright (C) 2011 Florian Haas <f.g.haas@gmx.net>

 This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License as
 published by the Free Software Foundation; either version 2 of
 the License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, contact:

 Free Software Foundation           Voice:  +1-617-542-5942
 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 Boston, MA  02110-1301,  USA       gnu@gnu.org
-->
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
  
