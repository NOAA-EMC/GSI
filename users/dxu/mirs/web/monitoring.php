<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Monitoring</title>
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
				
				<h1>Microwave Integrated Retrieval System (MIRS) - Monitoring</h1>

				<h2>Data Quality Monitoring</h2>

				<p>This is the Data Quality Monitoring (DQM) online section of the Microwave Integrated Retrieval System (MIRS).
				The Microwave Integrated Retrieval System (MIRS) runs routinely at NOAA/NESDIS/STAR for a number of sensors (currently
				NOAA-18 and METOP-A and soon for <a href="ssmis.php"> DMSP-F16 SSMIS</a>). 
				To assess the products quality, we routinely monitor both the
				geophysical and radiometric performances, in addition to monitoring the products themselves. This is done by using the
				<a href="http://aoc.amma-international.org/nwp/globalmodel/ncep/ncep-gdas/">Global Data Assimilation System (GDAS)</a> 
				Analyses outputs. These gridded fields are first interpolated to the exact
				location of the satellite measurements before a point-to-point comparison is undertaken. The geophysical performance is
				the comparison of the MIRS products with the GDAS products while the radiometric performances consist of the comparison
				between the measurements and the simulated radiances using the <a href="http://www.star.nesdis.noaa.gov/smcd/spb/CRTM/">
				Community Radiative Transfer Model (CRTM)</a> and the GDAS
				fields as inputs. It is important to note that at any given day at around 1AM, MIRS kicks off the processing of the
				previous day's data.</p>

				<h2>Scientific Monitoring</h2>

				<p>It is also important to keep in mind that MIRS uses five (5) analyzes (00, 06, 12, 18, 24) to perform the
				time-interpolation of GDAS fields  to avoid doing any extrapolation or using the 18h00 analysis for all data between
				18h00 and 24h00. This requires that the geophysical performance at any given day is processed only two days after that
				particular day.
				Besides the products monitoring, the geophysical and radiometric performances monitoring, we also perform the daily
				computation of noise values (NeDT) as they are used as inputs to the 1DVAR system, and monitor their time variation. The
				purpose being to detect any glitches, noise increases or any other type of trends. The same is done for the brightness
				temperatures calibration biases, slopes and intercepts.
				This section of the web site is linked to the <a href="../mspps/">Microwave Surface and Precipitation Products System (MSPPS)</a> official
				products. Switching between the MIRS outputs (1DVAR) and the MSPPS products (Heritage) is possible.</p>

				<h2>Seasonal/Climate Time series</h2>

				<p>For seasonal and climate applications, the time series of the products are also available in this section, allowing an
				animation of the fields of the products with different steps and increments to allow the user to detect any change in
				features with time.</p>

				<h2>Descriptions of the Options</h2>

				<p>This section of the web site allows the user to :</p>
				
				<ol>
				
				<li> Monitor the products derived from the Microwave Integrated Retrieval System (MIRS).</li>
				<li> Have access to the daily scientific validation monitoring, which is based on a point-by-point comparison to the
					Global Data Assimilation System (GDAS) outputs.</li>
				<li> Animate a time series of several products.</li>
				<li> Have access to the daily radiometric performances monitoring.</li>
				<li> View the NeDT time series to monitor any trend.</li>
				<li> View the biases, slopes, intercepts of the bias correction as a function of time to detect any time drifts, 
					for all scan positions.</li>
				</ol>


				<h2>Applications</h2>

				<p>For all options above, the user can select which sensor to view, which product to display, which channel to analyze, and
				when selecting an atmospheric profile (be it temperature, moisture, etc), which layer to visualize. When selecting
				emissivity, one can also select which particular channel to focus on. This flexible setting can allow the user to perform:</p>
				
				<ol>
				<li> Inter-sensor comparison (same product, different sensor)</li>
				<li> Cross-talk assessment (same sensor, different products)</li>
				<li> Time series (same sensor, same product, different dates)</li>
				<li> Diurnal variation assessment (same product, same sensor, but ascending versus descending)</li>
				<li> Cross-algorithm comparison (same sensor, same date, same product, different algorithms) etc...</li>
				</ol>

				<br>
				<p>Please click on one of the options on the left panel to start the DQM process.</p>





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
