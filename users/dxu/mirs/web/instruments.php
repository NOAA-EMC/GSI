<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - Microwave Integrated Retrieval System (MIRS) - Sensors</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<!-- if you need to include your own .css files
     put the links HERE, before the closing head tag. -->

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

			<h1>Microwave Integrated Retrieval System (MIRS) - Sensors</h1>

			<p>This sections describes the sensor characteristics (central frequency, 
			bandwidth, polarization, NeDT, etc) of the different instruments for which MIRS is either:</p>
			<ol>
			<li>routinely running (NOAA-18 and METOP-A AMSU and MHS)</li>
			<li>in the process of being applied to (DMSP-F16/F17 SSMIS) or</li>
			<li>planned to be applied to when the sensor is flying (such as NPP/NPOESS ATMS, NPOESS MIS, etc)</li>
			</ol>
			
			<p>
			<a href="polarization.php" target=_blank>
			Click here for polarization illustration.(V. Pol., H. Pol., RHCP, etc)</a>
			</p>

			<p>
			Please click on a specific sensor on the left panel to get access to the corresponding information.
			</p>


				<!-- END your project-specific content HERE -->
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
