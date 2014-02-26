<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - Microwave Integrated Retrieval System (MIRS) - Sensors - AMSUA</title>
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

				AMSR-E Overview</h1>

				<p>
				The Advanced Microwave Scanning Radiometer for EOS (AMSR-E) is a modified version 
        			of AMSR that flew on ADEOS-II. NASDA provides the instrument for 
        			flight on board NASA's Earth Observing System (EOS) Aqua platform. AMSR-E 
        			is indispensable for Aqua's mission, which is dedicated to the 
        			observation of climate and hydrology.</p>

				<p>
				AMSR-E is a multifrequency, dual-polarized microwave radiometer that 
        			detects faint microwave emissions from the Earth's surface and 
        			atmosphere. Various geophysical parameters can be retrieved, including 
        			water vapor, cloud liquid water, precipitation, sea surface temperature, 
        			sea surface wind speed, sea ice concentration, snow water equivalent, 
        			and soil moisture. Global and continuous observation is performed with 
        			fine spatial resolution by using one of the largest ever microwave 
        			radiometer antenna. The long-term geophysical record will play an 
        			important role in climate change monitoring and will provide valuable 
        			information for understanding the Earth's climate system, including 
        			water and energy circulation. Near real-time products will be used to 
        			investigate satellite data assimilation into weather forecasting models 
        			and to contribute to improved forecasting accuracy.</p>

				<p>	
				AMSR aboard ADEOS-II (AMSR-E aboard Aqua) will be placed in a morning 
        			(afternoon) orbit. Frequent sampling realized by this combination 
        			enables us to observe rapidly changing phenomena as well as to obtain 
        			information on the diurnal variation of geophysical parameters.</p>


				<p>	
				AMSR-E aboard Aqua is a joint project with NASA as well as an 
        			international activity in which scientists and engineers worldwide 
        			collaborate in algorithm development and data validation.</p>

				<p>	AMSR-E     
        			is a modified version of AMSR that flew aboard ADEOS-II and is intended     
        			to fly on the EOS Aqua platform. Major modifications include scaling     
        			down the antenna size (from 2.0m to 1.6m) due to the limitation of the     
        			fairing size, employing a deployable antenna system, and removing the     
        			50GHz channels. Since Aqua's orbit altitude of approximately 700km is     
        			lower than that of ADEOS-II, the spatial resolution remains nearly     
        			unaffected by this down sizing of the antenna. Information on the oxygen     
        			absorption line complex in the 50- to 60-GHz band is provided by the     
        			Advanced Microwave Sounding Unit (AMSU), one of the Aqua satellite     
        			mission instrument.</p>


        			<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
				<caption>AMSR-E Sensor Characteristics</caption>
        			  <tr>
        			    <th scope="row" rowspan="2">Center Frequency (GHz)</th>     
        			    <td align="center" rowspan="2">6.925</td> 
        			    <td align="center" rowspan="2">10.65</td> 
        			    <td align="center" rowspan="2">18.7</td> 
        			    <td align="center" rowspan="2">23.8</td> 
        			    <td align="center" rowspan="2">36.5</td> 
        			    <td align="center">89.0</td> 
        			    <td align="center">89.0</td> 
        			  </tr> 
        			  <tr> 
        			    <td align="center">A</td> 
        			    <td align="center">B</td> 
        			  </tr> 
        			  <tr> 
        			    <th scope="row">Band Width (MHz)</th> 
        			    <td align="center">350</td> 
        			    <td align="center">100</td> 
        			    <td align="center">200</td> 
        			    <td align="center">400</td> 
        			    <td align="center">1000</td> 
        			    <td align="center" colspan="2">3000</td> 
        			  </tr> 
        			  <tr> 
        			    <th scope="row">Polarization</th> 
        			    <td colspan="7" align="center">Vertical and Horizontal</td>  
        			  </tr> 
        			  <tr> 
        			    <th scope="row">3dB Beam Width (&#176;)</th> 
        			    <td align="center">2.2</td> 
        			    <td align="center">1.5</td> 
        			    <td align="center">0.8</td> 
        			    <td align="center">0.92</td> 
        			    <td align="center">0.42</td> 
        			    <td align="center">0.19</td> 
        			    <td align="center">0.18</td> 
        			  </tr> 
        			  <tr> 
        			    <th scope="row">IFOV (km)</th> 
        			    <td align="center">43&#215;75</td> 
        			    <td align="center">29&#215;51</td> 
        			    <td align="center">16&#215;27</td> 
        			    <td align="center">18&#215;32</td> 
        			    <td align="center">8.2&#215;14.4</td> 
        			    <td align="center">3.7&#215;6.5</td> 
        			    <td align="center">3.5&#215;5.9</td> 
        			  </tr> 
        			  <tr> 
        			    <th scope="row">Sampling Interval (km)</th> 
        			    <td colspan="5" align="center">9&#215;10</td> 
        			    <td align="center">4.5&#215;4</td> 
        			    <td align="center">4.5&#215;6</td> 
        			  </tr> 
        			  <tr> 
        			    <th scope="row">Temperature Sensitivity (K)</th> 
        			    <td align="center">0.34</td> 
        			    <td align="center">0.7</td> 
        			    <td align="center">0.7</td> 
        			    <td align="center">0.6</td> 
        			    <td align="center">0.7</td> 
        			    <td align="center">1.2</td> 
        			    <td align="center">1.2</td> 
        			  </tr> 
        			  <tr> 
        			    <th scope="row">Incidence Angle (&#176;)</th> 
        			    <td colspan="6" align="center">55.0</td> 
        			    <td align="center">54.5</td> 
        			  </tr> 
        			  <tr> 
        			    <th scope="row">Dynamic Range (K)</th> 
        			    <td colspan="7" align="center">2.7 - 340</td> 
        			  </tr> 
        			  <tr> 
        			    <th scope="row">Swath Width (km)</th> 
        			    <td colspan="7" align="center">Approximately 1450</td> 
        			  </tr> 
        			  <tr> 
        			    <th scope="row">Integration Time (msec)</th> 
        			    <td colspan="5" align="center">2.5</td> 
        			    <td align="center" colspan="2">1.2</td> 
        			  </tr> 
        			  <tr> 
        			    <th scope="row">Quantization (bit)</th> 
        			    <td align="center">12</td> 
        			    <td colspan="6" align="center">10</td> 
        			  </tr> 
        			  <tr> 
        			    <th scope="row">Scan Cycle (sec)</th> 
        			    <td colspan="7" align="center">1.5</td> 
        			  </tr> 
        			</table> 


				<br />
				<br />
				<hr>
				<br />
					<p>Source: <a href="http://aqua.nasa.gov/"> NASA AQUA Project</a></p>
					

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
