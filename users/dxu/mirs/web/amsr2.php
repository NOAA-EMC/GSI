<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - Microwave Integrated Retrieval System (MIRS) - Sensors - GCOM-W1/AMSR2</title>
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

				AMSR2 Overview</h1>
				<p>				
				The Advanced Microwave Scanning Radiometer 2 (AMSR2) 
				onboard the GCOM-W1 satellite is a remote sensing 
				instrument for measuring weak microwave emission 
				from the surface and the atmosphere of the Earth. 
				From about 700 km above the Earth, AMSR2 will provide 
				us highly accurate measurements of the intensity of 
				microwave emission and scattering.
				The antenna of AMSR2 rotates once per 1.5 seconds and 
				obtains data over a 1450 km swath. This conical scan 
				mechanism enables AMSR2 to acquire a set of daytime 
				and nighttime data with more than 99% coverage of the Earth every 2 days.</p>
				
				<ul> 
				<li>Deployable main reflector system with 2.0m diameter (1.6m for AMSR-E).
				</li>

				<li>
				Frequency channel set is identical to that
				of AMSR-E except 7.3GHz channel for RFI mitigation.
				</li>

				<li>	
				Two-point external calibration with
				improved HTS (hot-load).
				</li>

				<li>	
				Deep space calibration maneuver to
				check consistency between main reflector and CSM.
				</li>

				<li>
				Add a redundant momentum wheel to increase reliability.
				</li>

				</ul>


<table border="0" cellpadding="0" cellspacing="20">
<tr>

<td>
<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
<tr><th align=center height=36 colspan=2 ><font color="#6495ED">GCOM-W1/AMSR2 characteristics</font></th></tr>
<tr><th align=center height=36>Scan and rate  </th><td align=center height=36>Conical scan at 40 rpm</td></tr>
<tr><th align=center height=36>Antenna        </th><td align=center height=36>Offset parabola with 2.0m dia.</td></tr>
<tr><th align=center height=36>Swath width    </th><td align=center height=36>1450km</td></tr>
<tr><th align=center height=36>Incidence angle</th><td align=center height=36>Nominal 55 degrees</td></tr>
<tr><th align=center height=36>Digitization   </th><td align=center height=36>12bits</td></tr>
<tr><th align=center height=36>Dynamic range  </th><td align=center height=36>2.7-340K</td></tr>
<tr><th align=center height=36>Polarization   </th><td align=center height=36>Vertical and horizontal</td></tr>
</table>
</td>


<td>
<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
<tr><th align=center height=24 colspan=5><font color="#6495ED">AMSR2 Channel Set</font></th></tr>

<tr>
<th align=center height=69>Center<br>Freq.<br>[GHz]</th>
<th align=center height=69>Band<br>width<br>[MHz]</th>
<th align=center height=69><br>Pol.</th>
<th align=center height=69>Beam width [deg]<br>(Ground res. [km])</th>
<th align=center height=69>Sampling<br>interval<br>[km]</th>
</tr>

<tr><td align=center height=26>6.925</td><td align=center height=26>350 </td><td align=center height=26>V and H</td><td align=center height=26>1.8 (35 x 62) </td><td align=center height=26>10</td></tr>
<tr><td align=center height=26>7.30 </td><td align=center height=26>350 </td><td align=center height=26>V and H</td><td align=center height=26>1.8 (35 x 62) </td><td align=center height=26>10</td></tr>
<tr><td align=center height=26>10.65</td><td align=center height=26>100 </td><td align=center height=26>V and H</td><td align=center height=26>1.2 (24 x 42) </td><td align=center height=26>10</td></tr>
<tr><td align=center height=26>18.7 </td><td align=center height=26>200 </td><td align=center height=26>V and H</td><td align=center height=26>0.65 (14 x 22)</td><td align=center height=26>10</td></tr>
<tr><td align=center height=26>23.8 </td><td align=center height=26>400 </td><td align=center height=26>V and H</td><td align=center height=26>0.75 (15 x 26)</td><td align=center height=26>10</td></tr>
<tr><td align=center height=26>36.5 </td><td align=center height=26>1000</td><td align=center height=26>V and H</td><td align=center height=26>0.35 (7 x 12) </td><td align=center height=26>10</td></tr>
<tr><td align=center height=26>89.0 </td><td align=center height=26>3000</td><td align=center height=26>V and H</td><td align=center height=26>0.15 (3 x 5)  </td><td align=center height=26>5 </td></tr>

</table>
</td>


</tr>
</table>


				<hr>
				<br />
					<p>Source: <a href="http://suzaku.eorc.jaxa.jp/GCOM_W/w_amsr2/whats_amsr2.html">JAXA GCOM-W1 </a></p>
					

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
