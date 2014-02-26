<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Download DAP</title>
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
				
				<h2>Download MIRS DAP Version 9:</h2>
				<p>
				A license agreement is required to access the MIRS DAP and documentation.  
				Please download the MIRS license agreement in the link below, complete,
				sign, and email to <font color=blue>Xiwu.Zhan@noaa.gov</font>.  
				You will then be contacted and provided with download instructions once 
				the license agreement has been approved.  This will also put you in a 
				low-volume emailing list used to send updates about MIRS, known bugs, 
				improvements, new releases, etc.
				</p>

				<!--p>
				Registration is required to access the MIRS EDR and DEP file downloads.  
				Please send an email to Sid Boukabara: Sid.Boukabara@noaa.gov 
				with a request detailing your name, organization, and how we can reach you, 
				as well as your primary use of the data.  A username and password will then be sent to you. 
				</p-->
				
				<br>
				<a href="MIRS_LicenseAgreement_asOfMarch2013_V4_template.doc">download MIRS License Agreement</a> (Word document, 43KB)
				
				
				<br><br><br><br>
				
				<a href="download/mirs_v9r2_r3259_oper_20130815.tar.gz">MIRS Operational Release DAP Version 9.2</a> (195 MB,
				updated 08/15/2013)
				<br><br>
				
				<br><br><br><br><br>
				<p>The following is MiRS current version in baseline, only for research purpose. Several sensors/products 
				are not validated and should NOT be used as performance judgement of MiRS.</p>
				<a href="download/mirs_v9_r3245_res.tar.gz">MIRS Current Baseline Research DAP Version 9</a> (1.4 GB, updated 01/07/2013)
				
				
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
