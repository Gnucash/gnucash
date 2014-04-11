<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:qof-qsf="http://qof.sourceforge.net/" xmlns:str="http://xsltsl.org/string" version="1.1">
  <xsl:import href="string.xsl"/>
  <!-- our own gnucash standard routines -->
  <xsl:import href="gnucash-std.xsl"/>
  <xsl:output method="text"/>
  <!-- Representing QSF gncAddress data in a 2.1 extended VCard 

  This stylesheet converts the GnuCash output from Export Customers
  into a simple Vcard, suitable for import into KAdddressbook. For other 
  VCard support (e.g. for mobile phones), see other vcard XSL stylesheets in the 
  gnucash collection.

  Each VCard is written into a separate .vcf file, named after the 
  contact described in the gncAddress records. Spaces are replaced with
  underscores. Specify the '-o dir/' option to xsltproc to output all vcards
  into a directory.
  -->
  <!-- Licence

  This file is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
  -->
  <xsl:template match="/*/qof-qsf:book/qof-qsf:object[@type='gncAddress']">
    <xsl:text>TEL;TYPE=WORK:</xsl:text>
    <xsl:value-of select="qof-qsf:string[@type='phone']"/>
    <xsl:text>&#10;TEL;TYPE=FAX:</xsl:text>
    <xsl:value-of select="qof-qsf:string[@type='fax']"/>
    <xsl:text>&#10;EMAIL:</xsl:text>
    <xsl:value-of select="qof-qsf:string[@type='email']"/>
    <xsl:text>&#10;ADR;TYPE=work:;;</xsl:text>
    <xsl:value-of select="qof-qsf:string[@type='number']"/>
    <xsl:text>;</xsl:text>
    <xsl:value-of select="qof-qsf:string[@type='street']"/>
    <xsl:text>;</xsl:text>
    <xsl:value-of select="qof-qsf:string[@type='locality']"/>
    <xsl:text>;</xsl:text>
    <xsl:value-of select="qof-qsf:string[@type='city']"/>
    <xsl:text>&#10;N:</xsl:text>
    <xsl:value-of select="qof-qsf:string[@type='name']"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>
  <xsl:template match="/">
    <xsl:for-each select="/*/qof-qsf:book/qof-qsf:object[@type='gncCustomer']">
      <xsl:variable name="contactName">
        <xsl:value-of select="qof-qsf:string[@type='name']"/>
      </xsl:variable>
      <xsl:variable name="card_title">
        <xsl:call-template name="get_chunk_name">
          <xsl:with-param name="entryName" select="$contactName"/>
        </xsl:call-template>
      </xsl:variable>
      <!-- chunking support -->
      <xsl:document href="{$card_title}.vcf" method="text">
        <xsl:text>BEGIN:VCARD&#10;VERSION:2.1&#10;</xsl:text>
        <xsl:call-template name="locate_child">
          <xsl:with-param name="customer_object" select="."/>
        </xsl:call-template>
        <xsl:text>END:VCARD&#10;</xsl:text>
      </xsl:document>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
