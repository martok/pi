<xsl:stylesheet
  version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
 <xsl:output method="html"
  media-type="text/html" encoding="utf-8"
  omit-xml-declaration="yes"
  doctype-public="-//W3C//DTD XHTML 1.1//EN"
  doctype-system="http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"/>

 <!-- This is an identity template - it copies everything
      that doesn't match another template -->
 <xsl:template match="@* | node()">
   <xsl:copy>
     <xsl:apply-templates select="@* | node()"/>
   </xsl:copy>
 </xsl:template>
 
 <xsl:template match="/document">
   <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="de" lang="de" dir="ltr">
       <head>
         <meta content="text/html; charset=UTF-8" http-equiv="Content-Type" />
         <title><xsl:value-of select="title"/></title>
         <style type="text/css">
            body {
                background-color: #ffffe0;
                margin: 0;
                padding: 5px;
                line-height: 1.5;
                font-family: Verdana,Arial,sans-serif;
                font-size: 14px;
            }

            a,a:visited,a:active {
            	color: #555;
            	border-top:1px solid #555;
            	border-bottom:1px solid #555;
            	text-decoration:none;
            }
            h1 {
            	margin-top: 50px;
            }

            h1#main {
            	margin-top: 0px;
            }

            code {
            	border: 1px solid silver;
            	padding: 2px;
            	margin: 1px;
            }
            h3 code,
            pre code {
            	border: none;
            	margin: 0;
            	padding: 0;
            }
            pre {
            	margin-left: 20px;
            	padding-left: 1ex;
            	border: 1px solid silver;
            	background: white;
            }

            li {
            	margin-bottom: 10px;
            }
            
            dd p, li p {
            	margin-top: 0.2em;
            }

            a.function-link {
            	float: right;
            	border: 0px;
			}
            li.function {
            	margin-bottom: 2em;
			}

            span.function-options {
            	text-decoration: underline;
			}
            
			dl.function-options dt {
            	font-family: monospace;
            	padding-left: 1em;
            }
			dl.function-options dd table {
            	border-left: 1px solid black;
            	width: 100%;
            }
			dl.function-options dd table td {
            	border-bottom: 1px solid #bbb;
            }
         </style>
       </head>
       <body>
         <xsl:apply-templates/>
       </body>
     </html>
 </xsl:template>
 <xsl:template match="title">
    <h1><xsl:value-of select="."/></h1>
 </xsl:template>
 <xsl:template match="section">
    <h2><xsl:value-of select="@title"/></h2>
    <div><xsl:apply-templates /></div>
 </xsl:template>
 <xsl:template match="//syntax/v|//description/v">
   <xsl:element name="i"><xsl:apply-templates /></xsl:element>
 </xsl:template>
 <xsl:template match="description">
   <xsl:element name="p"><xsl:apply-templates /></xsl:element>
 </xsl:template>
 <xsl:template match="syntax">
   <code><xsl:apply-templates /></code><br />
 </xsl:template>
 <xsl:template match="seealso">
   <xsl:element name="a">
     <xsl:attribute name="href">
       <xsl:choose>
         <xsl:when test="./@n">
		   <xsl:text>#function-</xsl:text><xsl:value-of select="lower-case(@p)" /><xsl:text>-</xsl:text><xsl:value-of select="lower-case(@n)" />
		 </xsl:when>
		 <xsl:otherwise>
		   <xsl:text>#package-</xsl:text><xsl:value-of select="lower-case(@p)" />
		 </xsl:otherwise>
	   </xsl:choose>
     </xsl:attribute>
     <xsl:value-of select="." />
   </xsl:element>
 </xsl:template>
 <xsl:template match="package">
    <h3>
      <xsl:attribute name="id">
        <xsl:text>package-</xsl:text><xsl:value-of select="lower-case(@name)" />
      </xsl:attribute>
      <xsl:text>Package </xsl:text><code><xsl:value-of select="@name"/></code>
    </h3>
    <xsl:if test="count(./description) > 0">
      <xsl:apply-templates select="./description" />
    </xsl:if>
    <ul><xsl:apply-templates select="./function"/></ul>
 </xsl:template>
 <xsl:template match="package/function">
    <li class="function">
      <xsl:variable name="id">
        <xsl:text>function-</xsl:text><xsl:value-of select="lower-case(../@name)" /><xsl:text>-</xsl:text><xsl:value-of select="lower-case(@name)" />
      </xsl:variable>
      <xsl:attribute name="id"><xsl:value-of select="$id" /></xsl:attribute>
      <a class="function-link">
      	<xsl:attribute name="href"><xsl:text>#</xsl:text><xsl:value-of select="$id"/></xsl:attribute>
      	<xsl:text>#</xsl:text>
      </a>
      <xsl:apply-templates select="./syntax|./description|./options" />
    </li>
 </xsl:template>
 <xsl:template match="package/function/options">
    <xsl:if test="count(./option) > 0">
      <span class="function-options">Optionen:</span>
      <dl class="function-options">
        <xsl:for-each select="./option">
          <dt><xsl:value-of select="@name" /> = <xsl:value-of select="@default" /></dt>
          <dd><xsl:apply-templates select="./description" />
            <xsl:if test="count(./value) > 0">
            <table>
              <xsl:for-each select="./value">
                <tr>
                  <td class="option-value"><xsl:value-of select="@value" /></td>
                  <td><xsl:value-of select="." /></td>
                </tr>
              </xsl:for-each>
            </table>
            </xsl:if>
          </dd>
        </xsl:for-each>
      </dl>
    </xsl:if>
 </xsl:template>
</xsl:stylesheet>