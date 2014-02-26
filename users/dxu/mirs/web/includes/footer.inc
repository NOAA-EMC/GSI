	<tr>
		<td colspan="2">
			<div id="bottomBanner">
				<div class="left">
				<?php
				// this PHP function will render the date of the last change
				// to the web page in which it is embedded
				// please don't change it; if you want it to look different, 
				// I will be happy to help!
				// lori.brown@noaa.gov
				$unixts = getlastmod(); //last modified date/time for this web page as Unix timestamp
				echo "Last modified on " . date ("F j, Y g:m A",$unixts); //ex: January 15, 2004 10 AM 
				echo "<br>";
				// echo "http://www.star.nesdis.noaa.gov" . $_SERVER['PHP_SELF'];
				// echo "<br>";
				?>
				<a href="http://www.doc.gov/">US Dept. of Commerce</a>&nbsp;|&nbsp;<a 
					href="http://www.noaa.gov/"
					title="National Oceanographic and Atmospheric Administration">NOAA</a>&nbsp;|&nbsp;<a 
					href="http://www.nesdis.noaa.gov/"
					title="National Environmental Satellite, Data, and Information Service">NESDIS</a>&nbsp;|&nbsp;<a 
					href="http://www.star.nesdis.noaa.gov/"
					title="Center for Satellite Applications and Research">STAR</a><img src="images/1pix.gif" 
					alt=""></div>
				<div class="right"><a href="http://www.w3.org/WAI/WCAG1A-Conformance"
      		title="Explanation of Level A Conformance"><img 
      		height="32" width="88" align="right"
      		style="text-align: right; 
      		padding: 5px 0px 8px 8px;
      		line-height: 100%; 
      		font-size: 0.9em;"
          	src="http://www.w3.org/WAI/wcag1A.gif"
       	alt="Level A conformance icon, W3C-WAI Web Content Accessibility Guidelines 1.0"></a><a 
		href="http://validator.w3.org/check?uri=referer"
      		title="This page validates as HTML 4.01 Transitional."><img 
      		height="32" width="88" align="right"
      		style="text-align: right; 
      		padding: 5px 0px 8px 8px;
      		line-height: 100%; 
      		font-size: 0.9em;"
	       src="http://www.w3.org/Icons/valid-html401"
       	alt="Valid HTML 4.01"></a>&nbsp;<a href="http://www.noaa.gov/privacy.html">Privacy</a>&nbsp;|&nbsp;<a 
			href="http://www.star.nesdis.noaa.gov/star/productdisclaimer.php">Link&nbsp;&amp; Product&nbsp;Disclaimers</a>&nbsp;|&nbsp;<a 
			href="http://www.cio.noaa.gov/Policy_Programs/info_quality.html">Information&nbsp;Quality</a><img src="images/1pix.gif" 
			alt=""><br>
			<a href="http://www.star.nesdis.noaa.gov/star/accessibility.php">Accessibility</a>&nbsp;|&nbsp;<a 
			href="mailto:NESDIS.STAR.Webmaster@noaa.gov">Contact STAR webmaster</a>&nbsp;|&nbsp;<a 
			href="mailto:Wanchun.Chen@noaa.gov">Contact MIRS webmaster</a></div>
		<div class="clearing"></div>
			</div>
			<?php
			// please don't disturb the
			// following javascript include, 
			// it makes the navigation menu
			// expand and contract
			?>
			<script language="JavaScript" src="js/menuControllerPHP.js" type="text/javascript"></script>
		</td>
  </tr>
