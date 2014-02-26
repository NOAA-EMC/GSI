<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - - Sensors - WindSat</title>
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



				<h1>Sensors<br />
				WindSat Overview</h1>



				<p>
				WINDSAT is a joint IPO/DoD/NASA risk reduction demonstration project intended 
				to measure <b>ocean surface wind speed </b>and <b>wind direction</b> from space using a polarimetric 
				radiometer.</p> 

				<p>
				 WindSat is a polarimetric microwave radiometer developed by the U.S. Navy and the 
				 National Polar-orbiting Operational Enviromental Satellite System (NPOESS) 
				 Integrated Program Office (IPO) for measuring ocean surface wind speed and 
				 direction. WindSat will demonstrate the viability of using polarimetry to 
				 measure the wind vector from space and provide operationally usable tactical 
				 information to Navy units. The payload provides risk reduction data that the 
				 NPOESS IPO will use in the development of the Conical Microwave Imager Sounder 
				 (CMIS). WindSat is the primary payload on the DoD Space Test Program's Coriolis Mision.</p> 
				<p>

				The WindSat payload is a multi-frequency polarimetric radiometer nominally operating 
				at 6.8, 10.7, 18.7, 23.8, and 37 GHz. Using a conically-scanned 1.83 m offset 
				parabolic reflector with multiple feeds, WindSat will cover a 1025 km active 
				swath (based on an altitude of 830 km) and provide both fore and aft views of 
				the swath. The horizontal ground resolution is determined by the size of the 
				antenna and the altitude. The relatively large WindSat antenna will result in 
				spatial resolutions approximately three times greater than current systems, 
				improving the utility of the imagery data and enabling better measurements in 
				the littoral. The 10.7, 18.7, and 37.0 GHz channels are fully polarimetric; 
				that is, they derive all four Stokes parameters by measuring the six principal 
				polarizations. The 6.8 GHz channel is dual polarimetric (vertical and horizontal)
				and provides sea surface temperature as a secondary product. The 23.8 GHz channel 
				is also dual polarimetric because its purpose is to correct for atmospheric water 
				vapor which is unpolarized. </p> 

				<p>
				The radiometer operates in discrete bands at 6.8, 10.7, 18.7, 23.8, and 37.0 GHz. 
				Table 1 provides key design and performance parameters of the system. The 10.7, 18.7, 
				and 37.0 GHz channels are fully polarimetric. The 6.8 channel is dual-polarization 
				(vertical and horizontal), and is more sensitive to sea surface temperature (SST) than 
				to winds. Thus it is used to remove measurement noise due to variations in SST. Similarly, 
				the 23.8 channel has dual-polarization. This frequency responds strongly to water vapor 
				in the atmosphere, which attenuates the signal from the ocean surface. 
				Thus, 23.8 channel data mitigates the effects of the water vapor.
				</p>

				<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
				<caption>WindSat Description</caption>

				<tr>
				<th scope="col">Freq(GHz)</th> 
				<th scope="col">Channels</th> 
				<th scope="col">Band Width(MHz)</th> 
				<th scope="col">&#964;(ms)</th> 
				<th scope="col">NE&#8710;T(1)</th> 
				<th scope="col">Earth Incidence <br>Angle (deg)</th> 
				<th scope="col">IFOV(km)</th> 
				</tr>

				<tr><td align=center>6.8 </td> <td align=center>V,H</td> 	  	<td align=center>125</td>  <td align=center>5.00</td> <td align=center>0.48</td> <td align=center>53.5</td> <td align=center>40X60</td> </tr>
				<tr><td align=center>10.7</td> <td align=center>V,H &#177;45,L, R</td> 	<td align=center>300</td>  <td align=center>3.50</td> <td align=center>0.37</td> <td align=center>49.9</td> <td align=center>25X38</td> </tr>
				<tr><td align=center>18.7</td> <td align=center>V,H &#177;45,L, R</td> 	<td align=center>750</td>  <td align=center>2.00</td> <td align=center>0.39</td> <td align=center>55.3</td> <td align=center>16X27</td> </tr>
				<tr><td align=center>23.8</td> <td align=center>V,H</td> 		<td align=center>500</td>  <td align=center>1.48</td> <td align=center>0.55</td> <td align=center>53.0</td> <td align=center>12X20</td> </tr>
				<tr><td align=center>37.0</td> <td align=center>V,H &#177;45,L, R</td> 	<td align=center>2000</td> <td align=center>1.00</td> <td align=center>0.45</td> <td align=center>53.0</td> <td align=center>8X13</td> </tr>

				</table>

				V : Polarization vector is parallel to scan plane at nadir<br>
				H : Polarization vector is perpendicular to scan plane at nadir<br>
				&#177;45 : Linear Polarization<br>
				L: Left Circular Polarization<br>
				R: Right Circular Polarization<br>
				<br><br>
				<hr width=700>
				Source: <a href="http://www.nrl.navy.mil/content.php?P=04REVIEW87">NRL: WindSat - Remote Sensing of Ocean Surface Winds</a>



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
