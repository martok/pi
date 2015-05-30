java -jar saxon9he.jar -s:master.xml -xi:on -xsl:transform_xml.xsl -o:documentation.xml
cmd.exe /c "php cleanup.php < documentation.xml > ../bin/documentation.html"
del documentation.xml