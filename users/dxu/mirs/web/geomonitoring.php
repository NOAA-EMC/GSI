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
				
				<h1>Microwave Integrated Retrieval System (MIRS) - Geophysical Monitoring</h1>


				<p> The geophysical performance is the comparison of the MIRS products with other data sources. 
				</p>
				<br>
				
				
				<h2><a href="geonwp.php">Comparison to NWP ( GDAS and ECMWF )</a></h2>
				<p>GDAS:</p>
				<p>This is done by using the
				<a href="http://aoc.amma-international.org/nwp/globalmodel/ncep/ncep-gdas/">
				Global Data Assimilation System (GDAS)</a> analyses outputs. 
				These gridded fields are first interpolated to the exact
				location of the satellite measurements before a point-to-point comparison 
				is undertaken. It is also important to keep in mind that MIRS uses five (5) 
				analyzes (00, 06, 12, 18, 24) to perform the
				time-interpolation of GDAS fields  to avoid doing any extrapolation or 
				using the 18h00 analysis for all data between
				18h00 and 24h00. This requires that the geophysical performance at any given day 
				is processed only two days after that particular day.  
				</p>
				<br>
				
				
				<p>ECMWF:</p>
				<p>This is to compare MIRS products with European Centre for Medium-Range Weather 
				Forecasts (ECMWF) model outputs. For more information of ECMWF,
				please check ECMWF model website: <a href="http://www.ecmwf.int/">ECMWF</a>
				</p>
				<br>
				
				<!--p>GFS:</p>
				<p>
				This is to compare MIRS products with correspinding products in NCEP GFS forecast model outputs.
				GFS data used to be called AVN data.Please check NCEP GFS output: 
				<a href="http://www.nco.ncep.noaa.gov/pmb/nwprod/analysis/npac/gfs/">GFS model output </a>.
				</p-->
				
				
				<h2><a href="geomspps.php">Comparison to MSPPS </a></h2>
				<p>This is to compare MIRS advanced algorithm with heritage algorithm MSPPS. For more
				information of MSPPS, please check <a href="../mspps/"> MSPPS website. </a>.
				</p>
				<br>
				
				<h2><a href="geoamsre.php">Comparison with AMSR-E Ice and Snow Products</a></h2>
				<p>
				In order to assess the performance of the MIRS daily ice and snow 
				retrieval products comparisons are made with standard products 
				produced from Advanced Microwave Scanning Radiometer - Earth Observing 
				System (AMSR-E) brightness temperatures using algorithms developed 
				at NASA. The MIRS products that are compared are the total sea ice 
				concentration (SIC) and snow water equivalent (SWE).  In the case 
				of sea ice, two independent AMSR-E products are used for assessment: 
				the NASA Team 2, and the Bootstrap algorithm.  For each MIRS product, 
				several types of intercomparisons are presented. First, horizontal 
				maps of each day's retrievals, differences between MIRS and the 
				corresponding AMSR-E products, as well as scatterplots are shown.  
				In the case of sea ice concentration, difference maps and scatterplots 
				between the NASA Team 2 and Bootstrap are also provided to give an 
				estimate of the uncertainty in the AMSR-E products themselves. 
				The user is provided the option to display results on polar 
				stereographic or global latitude/longitude grids. Second, 
				time series containing daily values of various difference metrics 
				and skill scores are presented. This alows for the assessment of 
				performances over time. The user may display results for a single 
				sensor product, or for all sensors simultaneously. Third, 
				daily histograms comparing the MIRS with AMSR-E products are shown. 
				For both the time series and histograms, results for northern and 
				southern hemisphere sea ice concentration are displayed separately.
				</p>	
				<br>
				
				<h2><a href="geosnodas.php">Comparison with SNODAS Snow Products</a></h2>
				<p>
				Daily comparisons are made of MIRS retrieved snow water equivalent (SWE) with the 
				operational analyses produced from the NOAA National Weather Service National 
				Operational Hydrologic Remote Sensing Center (NOHRSC) SNOw Data Assimilation System 
				(SNODAS). The SNODAS analyses are produced once per day for the continental U.S. 
				valid at 0600 UTC, and are obtained directly from NOHRSC.  Assessments shown are maps, 
				scatterplots, histograms and time series of performance metrics 
				(e.g. bias, correlation, and Heidke skill score, etc.)
				</p>	
				<br>
				
				
				<h2><a href="geossmis.php">Comparison with SSMIS Sea Ice Products</a></h2>
				<p>
				Daily comparisons are made of MIRS retrieved total sea ice concentration (SIC) 
				with daily near real-time retrievals of SIC from DMSP F17 SSMIS brightness 
				temperatures using the NASA Team algorithm. The SSMIS near real-time estimates 
				are produced on a 25 km polar stereographic grid for both northern and southern 
				hemispheres and are obtained daily from the National Snow and Ice Data Center in Boulder, 
				Colorado. Assessments shown are maps, scatterplots, histograms and time series of 
				performance metrics (e.g. bias, correlation, and Heidke skill score, etc.)
				</p>	
				<br>
				
				
				<h2><a href="geobias.php">Performance Time Series </a></h2>
				<p>The daily updated time series of temperature and water vapor mean bias and standard deviation 
				is to help monitor MIRS performance. </p>
				<br>
				
				
				<h2><a href="qcgeo.php">Performance Tables </a></h2>
				<p>The daily updated table format of temperature and water vapor mean bias and standard deviation 
				.</p>
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
