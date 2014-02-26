<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - Validation</title>
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
				
				<h1>Microwave Integrated Retrieval System (MIRS) - Validation</h1>
				<br>

				<h2><a href="validationraob.php"> Comparison to Radiosonde</a></h2>
				<p>Daily collocations to radiosonde observations (RAOBs) are performed for each
				of the satellite sensors which MIRS processes. The entire set of
				daily RAOBs are provided by the NOAA PROducts and Validation System (NPROVS)
				RAOBs which fall within 50km and 2 hours of a satellite
				observation are accumulated in an archive, along with the collocated satellite
				observation itself. This section displays the results of
				comparing the MiRS retrievals with collocated RAOBs,
				including temperature sounding, humidity sounding and TPW.
				Other data which are included in the NPROVS dataset and collocated to the RAOBs are also 
				compared, including ATOVS, COSMIC, and GFS forecasts.
				<br>
				For NOAA-18, Metop-A, and DMSP F16 MiRS, the record of collocations extends from April 2008 - present. 
				For NOAA-19, the record extends from March 2009 - present.
				</p>
				<br>
		
		
				<h2><a href="validationhirs.php"> Comparison to HIRS</a></h2>
				<p>Along with radiosonde observations, the NPROVS dataset includes HIRS skin temperature. 
				MiRS skin temperature is compared to HIRS at all points
				where both are collocated to RAOBs. The record of collocations is equivalent to 
				that noted in the Comparison to Radiosonde section.</p>
				<br>


				<h2><a href="validationcloudsat.php"> Comparison to CloudSat</a></h2>
				<p>
				This section has been implemented to assess the performance of the MiRS hydrometors, 
				including the cloud liquid water path, rain water path, ice water path as well as the 
				rainfall rate, using the hydrometeors retrieved as part of the CloudSat mission. 
				The thresholds for collocation criterion are 50km and 3 minutes. All CloudSat 
				rainfall rates (1.5 km resolution) that fall within this range for a given MiRS field 
				of view are averaged to be more representative of the overall conditions within the MiRS 
				footprint (50km at nadir for POES/Metop and 70km at nadir for DMSP F16).
				</p>
				<br>
		
				<h2><a href="validationstage4timeseriesv.php"> Comparison to Stage IV</a></h2>
				<p>
				The hourly NCEP Stage IV precipitation analysis has been used as a reference 
				for the assessment and evaluation of the MiRS rainfall rate technique. 
				Produced by the 12 River Forecast Centers over the Continental U.S. (CONUS), 
				the NCEP Stage IV product is a 4-km resolution precipitation analysis derived 
				from hourly radar precipitation estimates and hourly rain gauge data. 
				The Stage IV precipitation analysis product is much more of an integrated hourly estimate,
				 while the satellite-based MiRS precipitation estimation products represent an 
				 instantaneous observation. Because of that, during the comparison, 
				 it has been assumed that the Stage IV rainfall rate is a constant amount 
				 for an entire hour and is compared with the MiRS precipitation estimates that had occurred within that hour. 
				</p>
				<br>
		
				<h2><a href="validationtrmm.php"> Comparison to TRMM</a></h2>
				<p>This section contains results from comparing MIRS rainfall rate to collocated 
				<a href="http://disc.sci.gsfc.nasa.gov/precipitation/documentation/TRMM_README/TRMM_2B31_readme.shtml"> TRMM 2B31 rainfall rate. </a>
				The thresholds for collocation
				criterion are 50km and 2 minutes. All TRMM rainfall rates (5km resolution) that fall 
				within this range for a given MiRS field of view are averaged to be more representative 
				of the overall conditions within the MiRS footprint (50km at nadir for POES/Metop and 70km at nadir for DMSP F16).
				<br>
				For NOAA-18, Metop-A, and DMSP F16 MiRS, the record of collocations extends from 
				December 2008 - present. For NOAA-19, the record extends from March 2009 - present.
				<br>
				**Note: Due to an instrument anomaly with TRMM PR, the period between June 2009 - present contains no collocations.

				
				</p>
				<br>
		
				<h2><a href="validationtrmm2a12.php"> Comparison to TRMM_2A12</a></h2>
				<p>This section contains results from comparing MIRS rainfall rate and integrated hydrometeor amounts to 
				collocated 
				<a href="http://disc.sci.gsfc.nasa.gov/precipitation/documentation/TRMM_README/TRMM_2A12_readme.shtml"> TRMM 2A12 </a> data. 
				The thresholds for collocation criterion
				are 50km and 2 minutes. All TRMM scenes (5km resolution) that fall within this range 
				for a given MiRS field of view are averaged to be more
				representative of the overall conditions within the MiRS footprint 
				(50km at nadir for POES/Metop and 70km at nadir for DMSP F16). Here,
				comparisons are made to TRMM rainfall rate, integrated rain water path, 
				ice water path, and cloud liquid water.

				When clicking the <b><font color=lightblue>Profiles</font></b> button, 
				panels containing the MiRS hydrometeor profiles and collocated TRMM profiles are displayed, along with
				the derived rainfall rate found for the scene. The TRMM profiles 
				are interpolated to the MiRS vertical pressure grid. To change the date,
				parameter, or surface type, select from the dropdown menus and then click <font color=lightblue><b>Submit Query</b></font>
				to display the results.

				For NOAA-18, NOAA-19, Metop-A, and DMSP F16 MiRS, the record of collocations extends from July 2009 - present.
				
				</p>
				<br>
		
				<h2><a href="validationipwg.php"> IPWG Project</a></h2>
				<p>
				The IPWG is dedicated to perform large-scale validations and intercomparisons of daily rainfall
				 estimates from operational and semioperational satellite precipitation estimates against 
				 "ground truth" precipitation references based on the rain gauge and/or 
				 ground-based radar observations. More information about the IPWG validation/intercomparison 
				 can be found at <a href="http://cawcr.gov.au/projects/SatRainVal/validation-intercomparison.html">IPWG validation/intercomparison</a>
				</p>
				<br>
		
				<h2><a href="validationraingauge.php"> Comparison to Rain Gauge</a></h2>
				<p>
				For assessment purposes, the MiRS precipitation composite estimate given in millimeters per 
				day has been generated using the rainfall rate samples (given in millimeters per hour) 
				from three MiRS precipitation estimation products: MiRS NOAA-18, Metop-A, and DMSP-F16. 
				Computed from -60 to 60 degree latitudes and with a spatial resolution of 0.25 degree, 
				the daily MiRS precipitation composite has been included as part of the IPWG validation/intercomparison. 
				Currently, the daily MiRS precipitation composite estimate is being validated over CONUS, South America, 
				and Australia, and its incorporation to other regions is in progress. For the validation over 
				CONUS and South America, the daily MiRS precipitation composite estimate extends from 12:00 UTC to 12:00 UTC, 
				while for the validation over Australia, the MiRS composite extends from 00:00 UTC to 24:00 UTC. 
				In this section, rain gauge observations are used as the precipitation reference. 
				</p>
				<br>
		
				<h2><a href="validationrainradar.php"> Comparison to Rain Radar</a></h2>
				<p>
				In this section, comparisons between microwave-based precipitation algorithms and rain radar 
				observations are presented. This section is complementary of the Comparison to Rain Gauge validation section.
				</p>
				<br>
		
				<h2><a href="validationcpctimeseriesv.php"> Comparison to CPC</a></h2>
				<p>
				This section presents comparisons between the daily NOAA/NCEP Climate Prediction Center (CPC) 
				precipitation (based on rain gauge analysis) and the daily MiRS precipitation estimate given 
				in millimeters per day and generated from the instantaneous rainfall rates derived from 
				MiRS NOAA-18, Metop-A, and DMSP-F16 rainfall rate products. In this comparison, the limited 
				temporal and spatial coverage associated with the satellite sensors used to generate 
				the daily MiRS precipitation estimate product must be considered, compared to that of the rain gauges. 
				This comparison is limited to the CONUS.
				</p>
				<br>
		
				<h2><a href="downloadvalidation.php"> Performance Summary</a></h2>
				<p>This section presents some MiRS peformance summaries.</p>
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
