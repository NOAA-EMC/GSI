<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - Microwave Integrated Retrieval System (MIRS) - Sensors - MHS</title>
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

			<h1>Sensors<br />
			MHS Overview</h1>

			<p>
			The Microwave Humidity Sounder (MHS) is a self-calibrating microwave radiometer, 
			observing the Earth with a field of view of &#177;50 degrees across nadir, in five 
			frequency channels of the millimeter-wave band (89-190 GHz). MHS, together with 
			the complementary AMSU-A instruments, provides the operational microwave sounding 
			capability for the NOAA-N, -N' meteorological satellites.</p>
			
			<p>
			Channels at 157 GHz and around the 183 GHz water vapor absorption line provide a 
			humidity profile sounding capability, while the 89 GHz channel provides information 
			on surface temperature and emissivity (in conjunction with AMSU-A channels) and 
			detects cloud and precipitation contaminated pixels. The MHS instrument represents 
			an improvement to the AMSU-B radiometer on board previous NOAA satellites, while 
			providing continuity to its data. Two MHS instruments are planned to fly on 
			NOAA-N and -N' satellites, and three on the METOP satellite series of the European 
			Polar System (EPS). </p>
			
			<p>
			MHS is a cross-track, line-scanned instrument. Ninety contiguous scene resolution 
			cells are sampled in a continuous scan, covering 49.44444... degrees on each side 
			of the sub-satellite path, with an antenna beam width of 1.11111... degrees at half 
			power point. These scan patterns and geometric resolution translate to a 17-km 
			diameter cell at nadir from the 870 km nominal orbital altitude.</p>
			
			<p>
			A parabolic mirror is rotated to sample the Earth scene at 90 equidistant angular 
			positions, at a rate of three scans every eight seconds, and at the same time to 
			provide reference measurements against two calibration sources, i.e. an on-board 
			blackbody target and a view on free space. The radiation is then conveyed on four 
			feeds at 89, 157, 183 and 190 GHz via a "quasi-optical" arrangement of lenses, 
			dichroic plates and a polarizing beam splitter.</p>
			

			<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
			<caption>MHS Channels and Passband Characteristics*</caption>
			
				<tr>
				<th scope="col" >Channel<br>(See Note 1)</th>
				<th scope="col" >Central Frequency<br> (GHz)</th>
				<th scope="col" >No of Passbands </th>
				<th scope="col" >RF Bandwidth <br>(MHz)(See Note 2) </th>
				<th scope="col" >T (K)<br>(See Note 3) </th>
				<th scope="col" >Polarization<br>(See Note 4)</th>
				</tr>

				<tr>
				<td align="center">H1</td>
				<td align="center">89.0</td>
				<td align="center">1</td>
				<td align="center">2800</td>
				<td align="center">0.22</td>
				<td align="center">V</td>
				</tr>

				<tr><td align="center">H2</td>
				<td align="center">157.0</td>
				<td align="center">1</td>
				<td align="center">2800</td>
				<td align="center">0.34</td>
				<td align="center">V</td>
				</tr>

				<tr>
				<td align="center">H3</td>
				<td align="center">183.311 &#177; 1.0</td>
				<td align="center">2</td>
				<td align="center">2 x 500</td>
				<td align="center">0.51</td>
				<td align="center">H</td>
				</tr>

				<tr><td align="center">H4</td>
				<td align="center">183.311 &#177; 3.0</td>
				<td align="center">2</td>
				<td align="center">2 x 1000</td>
				<td align="center">0.40</td>
				<td align="center">H</td>
				</tr>

				<tr>
				<td align="center">H5</td>
				<td align="center">190.311</td>
				<td align="center">1</td>
				<td align="center">2200</td>
				<td align="center">0.46</td>
				<td align="center">V</td>
				</tr>

				<tr>
				</table>
				
				<h3>Notes:</h3>
				
				<ol>
				<li>The five MHS channels provide data continuity with AMSU-B channels 16 to 20, with some minor changes in
				frequency allocation and polarization, and improved performance.</li>
				
				<li>The quoted values for the maximum bandwidths are double-sideband values and represent the maximum permissible
				bandwidths at the 3 dB points.</li>
				
				<li>Ground measured values for the first flight model (NOAA-N).</li>
				
				<li>The V and H polarizations correspond respectively to electrical fields normal or parallel to the ground track
				at nadir, both rotating by an angle equal to the scan angle for off-nadir directions.</li>
				</ol>
				
				<p>* From <a href="http://www.ncdc.noaa.gov/oa/pod-guide/ncdc/docs/klm/html/c3/sec3-9.htm">NOAA KLM User's Guide</a>
			

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
