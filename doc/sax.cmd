java -jar saxon9he.jar -s:master.xml -xi:on -xsl:transform_xml.xsl -o:documentation.xml
php cleanup.php < documentation.xml > documentation.html
del documentation.xml