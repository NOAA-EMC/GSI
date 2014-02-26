<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - Microwave Integrated Retrieval System (MIRS) - Project Background</title>
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

				<h1>Microwave Integrated Retrieval System (MIRS) - Background</h1>

				<p>The Microwave Integrated Retrieval System (MIRS) is an upgrade to the regression-based microwave retrieval system known as 
				<a href="../mspps/">MSPPS
				(Microwave Surface and Precipitation Products System)</a> which lacked the profiling capability and was specific to a single instrument.
				Some of the objectives of MIRS are (1) to perform the retrieval in all-weather conditions and (2) over all-surface types, with a major
				benefit being the extension of the spatial coverage to critically important active regions.
				</p>


				<p>MIRS is based on an assimilation-type scheme (1DVAR) capable of optimally retrieving atmospheric and surface state parameters
				simultaneously. More details could be found under the 'Algorithm' section. Thanks to its dynamic memory design, it is capable of
				performing retrievals using different instrumental configurations. It is also envisioned that it will serve as a tool for performing
				sensor design studies and performances estimation for future satellites and concepts like the future MIS sensor onboard NPOESS or a
				potential Geostationary-based microwave sensor.</p>

				<p>Currently at NOAA/NESDIS/STAR, MIRS is applied routinely to NOAA-18 and METOP-A AMSU/MHS sensors pair and is being extended to run
				routinely with DMSP-F16 SSMI/S data. It is also expected that MIRS will be the retrieval algorithm for the future sensors: NPP/ATMS and
				NPOESS microwave sensors (MIS, ATMS).</p>

				<p>MIRS uses the <a href="http://www.star.nesdis.noaa.gov/smcd/spb/CRTM/"> Community Radiative Transfer Model (CRTM) </a>
				as its forward operator, leveraging therefore a large amount of effort being
				undertaken at the Joint Center for Satellite Data Assimilation (JCSDA).</p>

				<p>The direct outputs from MIRS include temperature, moisture and several hydrometeors atmospheric profiles, land surface temperature and
				emissivity (at all channels). From these core products are derived a set of secondary products using the Vertical Integration and
				Post-Processing (VIPP) process. These include: Total Precipitable Water (TPW), vertically integrated Cloud Liquid Water (CLW), Ice
				Water Path (IWP), Graupel-size ice Water Path (GWP), Rain Water Path (RWP). In addition, surface properties are also derived from the
				retrieved emissivities and associated skin temperature. These include Snow Water Equivalent (SWE), Sea Ice Concentration (SIC), Soil
				Wetness Index (SWI) or Soil Moisture, etc.</p>

				<p>MIRS algorithm generates new products; vertical profiles of cloud amount, precipitation, ice, snow and graupel and surface emissivity
				spectra. Although the accuracy of the hydrometeors vertical distribution is not great because of the lack of information content in the
				measured radiances used, it nevertheless sets the stage for a future capability to retrieve accurately liquid and frozen precipitations
				profiles (either by using more channels or by getting help from external data such forecast outputs).</p>

				<p>Because the same algorithm will eventually be used consistently across the platforms (different channels spectra, polar and
				geostationary orbits), the time series of these retrievals will thus be self consistent, making the resulting climate data records free
				of jumps due to changes in the algorithms.</p>



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
