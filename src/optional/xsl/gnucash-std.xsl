<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:qof-qsf="http://qof.sourceforge.net/" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:dt="http://xsltsl.org/date-time" xmlns:str="http://xsltsl.org/string" version="1.0" exclude-result-prefixes="qof-qsf html dt str">
  <xsl:import href="date-time.xsl"/>
  <xsl:import href="string.xsl"/>
  <!--This stylesheet contains standard templates for QOF QSF.-->
  <!--This is only a working prototype -->
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
  <!-- Translation support via external parameters -->
  <xsl:param name="html_lang">en</xsl:param>
  <xsl:param name="encoding">iso-8859-15</xsl:param>
  <!-- Convert gnc_numeric notation to an XSL number -->
  <xsl:template name="numeric_to_double">
    <xsl:param name="numeric_string"/>
    <xsl:variable name="before" select="substring-before($numeric_string, '/')"/>
    <xsl:variable name="after" select="substring-after($numeric_string, '/')"/>
    <xsl:variable name="numeric" select="$before div $after"/>
    <xsl:value-of select="number($numeric)"/>
  </xsl:template>
  <xsl:template name="get_chunk_name">
    <xsl:param name="entryName"/>
    <xsl:param name="entryCompany"/>
    <xsl:variable name="result">
      <xsl:choose>
        <xsl:when test="$entryName = ''">
          <xsl:call-template name="str:subst">
            <xsl:with-param name="text" select="$entryCompany"/>
            <xsl:with-param name="replace">
              <xsl:text> </xsl:text>
            </xsl:with-param>
            <xsl:with-param name="with">
              <xsl:text>_</xsl:text>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          <xsl:variable name="temp">
            <xsl:call-template name="str:subst">
              <xsl:with-param name="text" select="$entryName"/>
              <xsl:with-param name="replace">
                <xsl:text> </xsl:text>
              </xsl:with-param>
              <xsl:with-param name="with">
                <xsl:text>_</xsl:text>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:variable>
          <xsl:call-template name="str:subst">
            <xsl:with-param name="text" select="$temp"/>
            <xsl:with-param name="replace">
              <xsl:text>;</xsl:text>
            </xsl:with-param>
            <xsl:with-param name="with">
              <xsl:text>_</xsl:text>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:value-of select="$result"/>
  </xsl:template>
  <xsl:template name="prepare_address_div">
    <xsl:param name="address_object" select="."/>
    <xsl:comment>this address panel can be located precisely using CSS</xsl:comment>
    <xsl:text>&#10;</xsl:text>
    <div class="address">
      <h2>Customer address</h2>
      <p class="address_para">
        <xsl:text>&#10;</xsl:text>
        <b>
          <xsl:value-of select="qof-qsf:string[@type='entryCompany']"/>
        </b>
        <br/>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="qof-qsf:string[@type='entryAddress']"/>
        <br/>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="qof-qsf:string[@type='entryCity']"/>
        <br/>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="qof-qsf:string[@type='entryState']"/>
        <br/>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="qof-qsf:string[@type='entryZip']"/>
        <br/>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="qof-qsf:string[@type='entryCountry']"/>
        <br/>
        <br/>
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="qof-qsf:string[@type='entryPhone1']"/>
      </p>
    </div>
  </xsl:template>
  <xsl:template name="vcard_safe">
    <xsl:param name="address_string"/>
    <xsl:variable name="safe_string">
      <xsl:call-template name="str:subst">
        <xsl:with-param name="text" select="$entryCompany"/>
        <xsl:with-param name="replace">
          <xsl:text>,</xsl:text>
        </xsl:with-param>
        <xsl:with-param name="with">
          <xsl:text>\,</xsl:text>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:variable>
    <xsl:value-of select="$safe_string"/>
  </xsl:template>
  <!-- hierarchy support -->
  <xsl:template name="locate_child">
    <xsl:param name="customer_object"/>
    <xsl:variable name="cust_addr_guid">
      <xsl:value-of select="qof-qsf:guid[@type='addr']"/>
    </xsl:variable>
    <xsl:for-each select="/*/qof-qsf:book/qof-qsf:object[@type='gncAddress']">
      <xsl:variable name="addr_guid">
        <xsl:value-of select="qof-qsf:guid[@type='guid']"/>
      </xsl:variable>
      <xsl:if test="$addr_guid = $cust_addr_guid">
        <xsl:variable name="set" select="."/>
        <xsl:apply-templates select="$set"/>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
