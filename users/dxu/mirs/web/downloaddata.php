<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Download NOAA-18 EDR & DEP</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>
</head>
<body>
<?php
// insert banner rows
require('includes/banner.inc');

// insert gray link bar
require('includes/toolBar_withSearch.inc');
?>
  <tr>
    <td id="navCell">
			<?php
			// insert navigation div
			require('includes/Sample_NavDiv.inc');
			?>
		</td>
		<td class="mainPanel"><a name="skipTarget"></a><?php require('includes/noScriptWarning.inc'); ?>
			<div class="padding">
				<!-- DO NOT DELETE OR ALTER CODE ABOVE THIS COMMENT -->
				<!-- EXCEPT for the contents of the <title></title> TAG!! -->
				<!-- You can start project specific content HERE -->
				<h1>Download MIRS EDR and DEP</h1>
				
				<p>(POES low res. file size is about 400 MB, POES high res. is 3.2 GB, DMSP file size is about 300 MB)</p>
				
				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
				<caption>MIRS EDR & DEP of Past 7 Days</caption>
				<tr> 
					<th scope="col">NOAA-18<br>low res.</th> 
					<th scope="col">NOAA-19<br>low res.</th> 
					<th scope="col">Metop-A<br>low res.</th> 
					<th scope="col">Metop-B<br>high res.</th> 
					<th scope="col">F16 SSMIS<br>low res.</th>  
					<th scope="col">F18 SSMIS<br>low res.</th>  
					<th scope="col">NPP ATMS<br>high res.</th>  
				</tr> 
				
				<script language="javascript" type="text/javascript">
				    var now = new Date();

				    for(var i = 9; i > 2; i-- ) { 
				    	now.setDate(now.getDate()-1) ; // yesterday

				    	var year  = now.getFullYear();
				    	var month = now.getMonth()+1;  // month starting index is 0
				    	var day   = now.getDate();

				    	var mm = String(month);
				    	if ( month < 10 ) mm = "0" + mm;

				    	var dd = String(day);
				    	if ( day < 10 ) dd = "0" + dd;

					document.write("<tr>");
					document.write("<td align=center><a href=\"./data/n18_mirs_edr_dep_" + year + mm + dd + ".tar.gz\">" + year + "-" + mm + "-" + dd + "</a></td>");
					document.write("<td align=center><a href=\"./data/n19_mirs_edr_dep_" + year + mm + dd + ".tar.gz\">" + year + "-" + mm + "-" + dd + "</a></td>");
					document.write("<td align=center><a href=\"./data/m2_mirs_edr_dep_"  + year + mm + dd + ".tar.gz\">" + year + "-" + mm + "-" + dd + "</a></td>");
					document.write("<td align=center><a href=\"./data/m1_mirs_edr_dep_"  + year + mm + dd + ".tar.gz\">" + year + "-" + mm + "-" + dd + "</a></td>");
					document.write("<td align=center><a href=\"./data/f16_mirs_edr_dep_" + year + mm + dd + ".tar.gz\">" + year + "-" + mm + "-" + dd + "</a></td>");
					document.write("<td align=center><a href=\"./data/f18_mirs_edr_dep_" + year + mm + dd + ".tar.gz\">" + year + "-" + mm + "-" + dd + "</a></td>");
					document.write("<td align=center><a href=\"./data/npp_mirs_edr_dep_" + year + mm + dd + ".tar.gz\">" + year + "-" + mm + "-" + dd + "</a></td>");
					document.write("</tr>");
				
				    }
				    
				</script>
				
				
				</table>


				<!-- END your project specific content HERE -->
				<!-- DO NOT DELETE OR ALTER BELOW THIS COMMENT! -->
			</div>
		</td>
	</tr>
<?php
// insert footer & javascript include for menuController
require('includes/footer.inc');
?>
</table>
</body>
</html>
