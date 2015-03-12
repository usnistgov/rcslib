<?xml version="1.0" encoding="ISO-8859-1"?>
<!-- Edited with XML Spy v4.2 -->
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:template match="/">
<html>
<head><title>My Configuration</title></head>
<body>

<table border="1" >
<tr><th>Gain</th><th>Value</th></tr>
<tr><td>Proportional</td><td><xsl:value-of select="MY_CONF/p_gain"/></td></tr>
<tr><td>Integral</td><td><xsl:value-of select="MY_CONF/i_gain"/></td></tr>
<tr><td>Derivative</td><td><xsl:value-of select="MY_CONF/d_gain"/></td></tr>
</table>

</body>
</html>
</xsl:template>
</xsl:stylesheet>
