<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Simulated Performance</title>
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
		<td class="mainPanel"><a name="skipTarget"></a>
		
		<noscript><h3 class="noscriptWarning">
		Javascript is currently disabled on this computer. 
		Product animations on this page are generated with javascript.
		If you need static access to these images, please contact MIRS webmaster.</h3>
		</noscript>
		
			<div class="padding">
				<!-- DO NOT DELETE OR ALTER CODE ABOVE THIS COMMENT -->
				<!-- EXCEPT for the contents of the <title></title> TAG!! -->
				<!-- You can start project specific content HERE -->
				
				<h1>Microwave Integrated Retrieval System (MIRS) - Simulated Performance</h1>
				
				<p>This section contains results for results obtained within the MIRS system using 
				simulated brightness temperatures.  The simulation results produced here, which are 
				reproduced each day with the latest version of the MIRS system, have two main advantages: 
				(1) it permits the assessment of expected MIRS performances in the case when the "true" 
				atmospheric and surface conditions are known exactly, and without the complication of
				errors introduced from forward model uncertainties and biases, and (2) it provides a 
				tool for determining the impacts of any updates to the MIRS system on simulated 
				performances since each day's results are compared to the previous day's results 
				with the MIRS Team being alerted automatically if any difference are detected.
				Two different sets of simulation results are presented, corresponding to two 
				different sets of validation data.
				</p>
				<br>

				
				<h2><a href="simulate.php">Simulated Performance Using ECMWF60</a></h2>
				
				<p>Results in this section are produced by running MIRS on the standard "ECMWF-60" 
				data set. This is a data set produced at ECMWF with the goal of providing a realistic 
				distribution of atmospheric and surface conditions, accounting for seasonality, 
				as well as geographic location. In the results here, we use only profiles located 
				over ocean, which amounts to nearly 6000 profiles. Performances are simulated using 
				both clear and cloudy profiles, with results stratified accordingly. Results are 
				presented for temperature and water vapor profiles, as well as certain surface 
				parameters.  </p>
				<br>
				

				<h2><a href="simulate2.php">Simulated Performance Using ECMWF Analysis</a></h2>

				<p>Results in this section are produced using a complete set of ECMWF gridded analyses 
				(valid on 2009-02-15) along with footprint-matched SDRs from NOAA-18 for the same day.  
				Using the complete global NWP fields, allows for the production not only of global 
				atmospheric performance statistics, but also a stratification of estimated performance 
				by surface type and geographic location, including the production of global maps. 
				This can facilitate the highlighting of particular areas where performances can be 
				improved. Results are presented for temperature and water vapor performance, with 
				results stratified by surface type (land,ocean) and atmospheric conditions (clear, cloudy).
				</p>

				<br>

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
