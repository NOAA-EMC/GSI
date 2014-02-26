<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - Microwave Integrated Retrieval System (MIRS)</title>
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
	
				<h1>Microwave Integrated Retrieval System (MIRS) - Objectives</h1>

				<h2>Overarching Goal</h2>
				<p>
				The overarching objective of the Microwave Integrated Retrieval System (MIRS) is to contribute to the execution of the
				NOAA elements in the Integrated Earth Observation System (IEOS), which is a NOAA priority (Mission Support).</p>

				<h2>Mission Goal</h2>
				<p>MIRS aims at providing timely and effective acquisition and delivery of satellite-derived information that supports
				requirements from the Mission goal.
				The integrated and generic natures of MIRS make it possible to reduce significantly the amount of time spent developing
				retrieval algorithms for new sensors, which in turn reduces dramatically the time needed to transition to operations.</p>

				<h2>Scientific Objectives</h2>
				<p>Scientifically, the objectives of MIRS include:<br>
				(1) an improved temperature and moisture profiles retrieval, <br>
				(2) the extension of the retrieved products to non-standard surfaces including sea-ice and snow-covered land, <br>
				(3) the retrieval in all-weather conditions including cloudy and precipitating conditions and <br>
				(4) an improved set of retrieved surface properties whose derivation is based on the retrieved emissivities 
				    instead of directly from the brightness temperatures.</p>

				<h2>Climate Studies</h2>
				<p>MIRS retrievals have also the objective to be useful for climate studies. Their independence from Numerical Weather
				Prediction (NWP) models outputs, including from the surface pressure, and the consistent treatment of all sensors using
				virtually the same retrieval algorithm, make the derived products indeed ideal to study the trends of several products.
				The MIRS web site has been designed to allow the user to monitor changes/changes in several products simultaneously and
				from different sensors.</p>

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
