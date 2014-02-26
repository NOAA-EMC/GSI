<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Download</title>
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

				<h1>Microwave Integrated Retrieval System (MIRS) - Download</h1>

				<p>
				This section is user-password protected. You need to be registered and have a username and a password to get permission to
				download the contents.
				If you are not registered, please send an email to Sid.Boukabara@noaa.gov with a request detailing the organization, name, and
				how we can reach you.
				This will put you in a low-volume emailing list used to send updates about MIRS, found bugs, improvements, new releases, etc.
				</p>
				
				<br>
				
				<p>
				This section allows the download of:</p>
				<ol>
				<li>The Delivery Algorithm Package (DAP) containing the source code, makefiles, scripts, sample data, benchmark files, static
				files, etc. For a more detailed list of the content of the DAP, please refer to the documents, also downloadable.</li>
				<li>Most recent 7 days of scientific data retrieved by MIRS for both <a href="mhs.php">NOAA-18 and METOP-A</a>. Note that these data are of
				scientific nature and do not constitute the official NOAA products, distributed through <a href="http://www.osdpd.noaa.gov/">OSDPD</a>. 
				The MIRS scientific files are
				composed of (1) main core products solved for by the 1DVAR system (Environmental Data Records or EDRs) and Vertically-Integrated
				and Post-Processed (VIPP) data into the Derived EDR products (DEP). These are bundled together in the downloaded packages.</li>
				<li>Documents necessary to run MIRS properly: These include the User's manual, the Interface Control Document (ICD), the System
				Description Document (SDD), etc. </li>
				</p>
				</ol>
				
				<br>
				<p>Please click on the left panel options to choose which item to download.</p>





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
