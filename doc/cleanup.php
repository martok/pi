<?php
  //No, I do not want arbitrary namespaces inserted into my document, thanks.
  echo str_replace(array(' xmlns:xi="http://www.w3.org/2001/XInclude"', ' xmlns=""', '&#xD;'),'',file_get_contents("php://stdin"));
?>
