<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - MIRS Project - - Sensors - NPOESS CMIS</title>
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
			NPOESS MIS*</h1>


			<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
			<caption>CMIS Environmental Data Records (EDR) and Proxy Data Sensors</caption>
				<tr>
					<th scope="col">Priority</th>
					<th scope="col">CMIS EDR</th>
					<th scope="col">Key EDR</th>
					<th scope="col">TYPE</th>
					<th scope="col">Proxy Data Sensor</th>
				</tr>


				<tr>
					<td>1A</td>
					<td>Atmospheric Vertical Moisture Profile</td>
					<td>Key</td>
					<td>Atmosphere</td>
					<td>AMSU, SSMIS</td>
				</tr>

				<tr>
					<td>1A</td>
					<td>Soil Moisture</td>
					<td>Key</td>
					<td>Land</td>
					<td>WindSat</td>
				</tr>

				<tr>
					<td>1A</td>
					<td>Global Sea Surface Winds (Speed)</td>
					<td>Key</td>
					<td>Ocean</td>
					<td>WindSat</td>
				</tr>

				<tr>
					<td>2A</td>
					<td>Global Sea Surface Winds (Direction)</td>
					<td>Key</td>
					<td>Ocean</td>
					<td>WindSat</td>
				</tr>

				<tr>
					<td>2A</td>
					<td>Sea Surface Temperature</td>
					<td>Key</td>
					<td>Ocean</td>
					<td>WindSat</td>
				</tr>

				<tr>
					<td>2A</td>
					<td>Precipitable Water Vapor</td>
					<td>&nbsp;</td>
					<td>Atmosphere</td>
					<td>SSMIS</td>
				</tr>

				<tr>
					<td>2A</td>
					<td>Atmospheric Vertical Temperature Profile</td>
					<td>Key</td>
					<td>Atmosphere</td>
					<td>AMSU, SSMIS</td>
				</tr>

				<tr>
					<td>2A</td>
					<td>Cloud Liquid Water</td>
					<td>&nbsp;</td>
					<td>Cloud</td>
					<td>SSMIS</td>
				</tr>

				<tr>
					<td>2A</td>
					<td>Cloud Ice Water Path</td>
					<td>&nbsp;</td>
					<td>Cloud</td>
					<td>AMSU, SSMIS</td>
				</tr>

				<tr>
					<td>2A</td>
					<td>Total Water Content</td>
					<td>&nbsp;</td>
					<td>Atmosphere</td>
					<td>SSMIS</td>
				</tr>

				<tr>
					<td>2A</td>
					<td>Precipitation (Type, Rate)</td>
					<td>&nbsp;</td>
					<td>Atmosphere</td>
					<td>SSMIS</td>
				</tr>

				<tr>
					<td>2B</td>
					<td>Land Surface Temperature</td>
					<td>&nbsp;</td>
					<td>Land</td>
					<td>SSMIS</td>
				</tr>

				<tr>
					<td>2B</td>
					<td>Ice Surface Temperature</td>
					<td>&nbsp;</td>
					<td>Ocean</td>
					<td>SSMIS</td>
				</tr>

				<tr>
					<td>2B</td>
					<td>Sea Ice Characterization</td>
					<td>&nbsp;</td>
					<td>Ocean</td>
					<td>SSMIS</td>
				</tr>

				<tr>
					<td>3B</td>
					<td>Snow Cover/Depth</td>
					<td>&nbsp;</td>
					<td>Land</td>
					<td>SSMIS</td>
				</tr>

				<tr>
					<td>3B</td>
					<td>Global Sea Surface Wind Stress</td>
					<td>&nbsp;</td>
					<td>Ocean</td>
					<td>WindSat</td>
				</tr>

				<tr>
					<td>3B</td>
					<td>Pressure (Surface/Profile)</td>
					<td>&nbsp;</td>
					<td>Atmosphere</td>
					<td>SSMIS</td>
				</tr>



				<tr>
					<td>3B</td>
					<td>Cloud Base Height</td>
					<td>&nbsp;</td>
					<td>Cloud</td>
					<td>SSMIS</td>
				</tr>

				<tr>
					<td>3B</td>
					<td>Surface Type</td>
					<td>&nbsp;</td>
					<td>Land</td>
					<td>SSMIS</td>
				</tr>

				<tr>
					<td>3B</td>
					<td>Imagery</td>
					<td>Key</td>
					<td>Imagery</td>
					<td>WindSat, SSMIS</td>
				</tr>


			</table>
			
			
			<br>
			<br>

			

			<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
			<caption>EDR Requirements Summary</caption>

				<tr>
					<th scope="col" bgcolor=yellow>&nbsp;</th>
					<th scope="col" colspan=2>HCS(km)</th>
					<th scope="col" colspan=2>Performance <br>(U) uncertainty or (A) accuracy</th>
					<th scope="col" bgcolor=yellow>Measurement Range</th>

				</tr>

			
				<tr>
					<td><br><br><br>Atmospheric Vertical <br> Moisture Profile</td>
					<td colspan=2><br><br><br>15</td>
					<td>
					<b>Clear (U)</b><br>

					Surface to 600 mb<br>
					600 mb to 300 mb<br>
					300 mb to 100 mb<br>

					<b>Cloudy (U)</b><br>

					Surface to 600 mb<br>

					600 mb to 300 mb<br>
					300 mb to 100 mb<br>

					</td>
					<td>
					<br>
					18 % or 0.2 g/kg<br>
					22 % or 0.1 g/kg<br>

					22 % or 0.04 g/kg<br>

					 <br>

					20 % or 0.2 g/kg<br>
					40 % or 0.1 g/kg<br>
					40 % or 0.04 g/kg<br>
 				 
					</td>

					<td><br><br><br>0 - 30 g/kg</td>
				</tr>
			
				<tr>
					<td>Soil Moisture</td>
					<td colspan=2>40</td>
					<td colspan=2>8.5% (U)</td>
					<td>0 - 100%</td>

				</tr>


                                <tr>
                                        <td>Sea Surface Winds Speed</td>
                                        <td colspan=2>20</td>
                                        <td colspan=2>1 m/s (A)</td>
                                        <td>0 - 25 m/s</td>

                                </tr>

                                <tr>
                                        <td>Sea Surface Winds Direction</td>
                                        <td colspan=2>56 X 35</td>
                                        <td colspan=2>6 deg (A)</td>
                                        <td>0 - 360 deg</td>

                                </tr>


                                <tr>
                                        <td>Sea Surface Winds Temperature</td>
                                        <td colspan=2>50</td>
                                        <td colspan=2>0.2 K(A)</td>
                                        <td>271K - 313K</td>

                                </tr>

                                <tr>
                                        <td>Precipitable Water</td>
                                        <td colspan=2>25</td>     
                                        <td colspan=2>Ice free ocean -1 mm (A)
					<br>Land/ice - Greater of 5% or 2 mm (A)</td> 
                                        <td>0 - 75 mm</td>
                                </tr>

			
                                <tr>
                                        <td><br><br><br><br><br><br> Atmospheric Vertical<br> Temperature Profile</td>
					
                                        <td>
					  <br><br><br><br>     Surface to 20 mb
					  <br><br><br><br><br><br><br>  20 to 0.01 mb
					</td>     
                                        
                                        <td>
					  <br><br><br><br>40
					  <br><br><br><br><br><br><br>200
					</td>
					
					<td>
					  <b>Clear(U)</b>   <br>
					  Surface to 700 mb <br>
					  700 mb to 300 mb  <br>
					  300 mb to 30 mb   <br>
					  30 mb to 6 mb     <br>
					  6 mb to 1 mb      <br>
					  1 mb to 0.3 mb    <br>
					  0.3 mb to 0.01 mb <br>
					  
					  <b>Cloudy (U)</b> <br>
					  Surface to 700 mb <br>
					  700 mb to 300 mb  <br>
					  300 mb to 30 mb   <br>
					  30 mb to 6 mb     <br>
					  6 mb to 1 mb      <br>
					  1 mb to 0.3 mb    <br>
					  0.3 mb to 0.01 mb <br>
					</td>
					
                                        <td>
					  <br> 1.6 K/1 km layers
					  <br> 1.4 K/1 km layers
					  <br> 1.3 K/3 km layers
					  <br> 1.5 K/5 km layers
					  <br> 2.4 K/5 km layers
					  <br> 3.5 K/5 km layers
					  <br> 6.5 K/5 km layers

					  <br>
					  <br> 2 K/1 km layers
					  <br> 1.4 K/1 km layers
					  <br> 1.3 K/3 km layers
					  <br> 1.5 K/5 km layers
					  <br> 2.4 K/5 km layers
					  <br> 3.5 K/5 km layers
					  <br> 6.5 K/5 km layers
					
					</td> 
                                        
					<td><br><br><br><br><br><br><br><br>162 K - 335 K</td>
                                </tr>

			
				<tr>
					<td><br><br>Cloud Liquid Water</td>
					<td colspan=2><br><br>20</td>
					<td colspan=2>
					  <b>No precipitation (U)</b><br>
					  Ice free ocean - 0.08 kg/m<sup>2</sup><br>
					  Land/ice - 0.21 kg/m<sup>2</sup><br>
					  <b>Precipitation (U) </b><br>
					  Ice free ocean - 0.23 kg/m<sup>2</sup><br>
					  Land/ice - 0.45 kg/m<sup>2</sup>
					</td>
					<td><br><br> 0 - 5 kg/m<sup>2</sup></td>
				</tr>

			
				<tr>
					<td>Cloud Ice Water Path</td>
					<td colspan=2>50</td>
					<td colspan=2>25% (A)</td>
					<td>0.3 - 2.6 kg/m<sup>2</sup></td>
				</tr>

			
				<tr>
					<td><br><br>Total Water Content</td>
					<td colspan=2><br><br>20</td>
					<td colspan=2>
					  <b>Point Measurement: (U)</b><br>
					  Non-precipitating - 1.2 kg/m<sup>2</sup><br>
					  Precipitating - Greater of 2.2 kg/m<sup>2</sup> of 15%<br>
					  <b>Global Average (U)</b>:0.25 kg/m<sup>2</sup> 
					</td>
					<td>0 - 60 kg/m<sup>2</sup></td>
				</tr>

			
				<tr>
					<td>Precipitation (Type, Rate)</td>
					<td colspan=2>15</td>
					<td colspan=2>
					  Ice free ocean - Greater of 1 mm/hr or 10% (A)<br>
					  Land/ice - Greater of 1 mm/hr or 50% (A)
					</td>
					<td>
					  Rate: 0 - 50 mm/hr <br>
					  Type: Rain and Ice 
					</td>
				</tr>

				<tr>
					<td>Land Surface Temperature</td>
					<td colspan=2>50</td>
					<td colspan=2>0.5 K (A)</td>
					<td>213 K \u2013 343 K</td>
				</tr>
				
				<tr>
					<td>Ice Surface Temperature</td>
					<td colspan=2>25</td>
					<td colspan=2>3 K (U)</td>
					<td>213 K \u2013 275 K</td>
				</tr>
				
				<tr>
					<td>Sea Ice Age and <br>Ice Concentration</td>
					<td colspan=2><br>20</td>
					<td colspan=2>Probability of Correct Typing<br> (Ice Age) - 80%</td>
					<td>
					  Age: Ice Age<br>
					  Classes first year, multi-year<br>
					  1/10 to 10/10
					</td>
				</tr>
				
				<tr>
					<td> <br>Snow Cover/Depth</td>
					<td colspan=2> <br>20</td>
					<td colspan=2><br>20 % (snow/no snow) (U)</td>
					<td>
					  Snow Depth Ranges: > 0 cm <br>
					  (Any Snow Thickness)<br>
					  Measurement range 0-100%
					</td>
				</tr>
				
				<tr>
					<td><br><br>Surface Wind Stress</td>
					<td colspan=2><br><br>20</td>
					<td colspan=2>
					  0.015 N/m<sup>2</sup> for wind speed £ 5 m/s (U)<br>
					  a+bW+gW<sup>2</sup> for 5 m/s < wind speed <br>
					  (W) < 25 m/s (U) <br>
					  a = -6.58X10<sup>-4</sup> <br>
					  b = 2.28X10<sup>-3</sup> <br>
					  g = 1.83X10<sup>-4</sup> 
					</td>
					<td><br><br>0 - 2.2 N/m<sup>2</sup></td>
				</tr>
				
				<tr>
					<td>Pressure Profile</td>
					<td colspan=2>25</td>
					<td colspan=2>
					  0 - 10 km: 3% (A) <br>
					  10 - 30 km: 5% (A)
					</td>
					<td>10 - 1050 mb</td>
				</tr>
				
				<tr>
					<td>Cloud Base Height</td>
					<td colspan=2>25</td>
					<td colspan=2>2 km (U)</td>
					<td>0 - 15 km</td>
				</tr>
				
				<tr>
					<td>Fresh Water Ice</td>
					<td colspan=2>20</td>
					<td colspan=2>
					  Ice Edge boundary: 6 km (U) <br>
					  Ice Concentration: 0.08 (U)
					</td>
					<td>0/10 to 10/10 concentration</td>
				</tr>
				
				<tr>
					<td>Vegetation /Surface Type</td>
					<td colspan=2>20</td>
					<td colspan=2>Correct Typing Probability: 75 %</td>
					<td>8 CMIS aggregated types</td>
				</tr>
				
				<tr>
					<td><br>Imagery</td>
					<td colspan=2>
					  Consistent with related EDRs<br>
					  (15, 20, 25, 40, 50, 56X25, 84X53)
					</td>
					<td colspan=2><br>Derived</td>
					<td>Dynamic range of all <br> measurement channels</td>
				</tr>
				

			</table>
			
			<br><br>
			
			<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
			<caption>CMIS Channel Beam Parameters</caption>
				<tr>
					<th scope="col" bgcolor=yellow>Channel</th>
					<th scope="col" bgcolor=yellow>HPBW [deg] (1)<br>across x along scan</th>
					<th scope="col" bgcolor=yellow>IFOV [Km]  (2)<br>across x along</th>
					<th scope="col" bgcolor=yellow>EFOV [Km]  (2)<br>across x along</th>
					<th scope="col" bgcolor=yellow>EIA [degrees] (1)</th>
				</tr>
			
			
				<tr>
					<td>6V, 6H</td>
					<td>1.632 x 1.659</td>
					<td>67.7 x 37.8</td>
					<td>67.7 x 39.3</td>
					<td>55.75</td>
				</tr>
			
				<tr>
					<td>10V, 10H</td>
					<td>0.986 x 0.972</td>
					<td>45.5 x 24.2</td>
					<td>45.5 x 24.8</td>
					<td>58.13</td>
				</tr>
			
				<tr>
					<td>10R, 10L</td>
					<td>0.98 x 0.97</td>
					<td>45.5 x 24.2</td>
					<td>45.5 x 24.8</td>
					<td>58.13</td>
				</tr>
			
				<tr>
					<td>18V, 18H</td>
					<td>0.595 x 0.646</td>
					<td>23.4 x 15.3</td>
					<td>23.5 x 15.5</td>
					<td>53.64</td>
				</tr>
				
			
				<tr>
					<td>18P, 18M</td>
					<td>0.598 x 0.614</td>
					<td>23.4 x 15.3</td>
					<td>23.5 x 15.5</td>
					<td>53.64</td>
				</tr>
			
				<tr>
					<td>18R, 18L</td>
					<td>0.637 x 0.66</td>
					<td>23.4 x 15.3</td>
					<td>23.5 x 15.5</td>
					<td>53.64</td>
				</tr>
			
				<tr>
					<td>23V, 23H</td>
					<td>0.569 x 0.619</td>
					<td>23.4 x 15.3</td>
					<td>23.5 x 15.5</td>
					<td>55.64</td>
				</tr>
			
				<tr>
					<td>36V, 36H</td>
					<td>0.411 x 0.419</td>
					<td>16.6 x 9.9</td>
					<td>16.7 x 10.3</td>
					<td>55.75</td>
				</tr>
				
			
				<tr>
					<td>36P, 36M</td>
					<td>0.411 x 0.419</td>
					<td>16.6 x 9.9</td>
					<td>16.7 x 10.3</td>
					<td>55.75</td>
				</tr>
			
				<tr>
					<td>60V complex</td>
					<td>0.36 x 0.315</td>
					<td>15.0 x 7.7</td>
					<td>14.9 x 8.2</td>
					<td>55.75</td>
				</tr>
			
				<tr>
					<td>60L</td>
					<td>0.371 x 0.319</td>
					<td>15.0 x 7.7</td>
					<td>14.9 x 8.2</td>
					<td>55.75</td>
				</tr>
			
				<tr>
					<td>89V, 89H</td>
					<td>0.377 x 0.314</td>
					<td>15.6 x 7.8</td>
					<td>15.3 x 8.1</td>
					<td>55.75</td>
				</tr>
				
			
				<tr>
					<td>166V</td>
					<td>0.359 x 0.349</td>
					<td>14.6 x 8.4</td>
					<td>14.6 x 8.8</td>
					<td>55.49</td>
				</tr>
			
				<tr>
					<td>183V complex</td>
					<td>0.355 x 0.371</td>
					<td>16.1 x 8.7</td>
					<td>16.1 x 9.0</td>
					<td>55.49</td>
				</tr>
			
			
			</table>
			
			<br><br>



			<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
			<caption>CMIS Baseline Channel Specifications</caption>
				<tr>
					<th scope="col" bgcolor=yellow>CHANNEL NAME</th>
					<th scope="col" bgcolor=yellow>CENTER FREQUENCY [GHz]</th>
					<th scope="col" bgcolor=yellow>POLARIZATION</th>
					<th scope="col" bgcolor=yellow>NEDT [K]</th>
					<th scope="col" bgcolor=yellow>BANDWIDTH [MHz]</th>
				</tr>
			
				<tr>
					<td> 6V </td>
					<td> 6.625 </td>
					<td> V </td>
					<td> 0.4 </td>
					<td> 350 </td>
				</tr>

				<tr>
					<td> 6H  </td>
					<td> 6.625 </td>
					<td> H </td>
					<td> 0.4 </td>
					<td> 350 </td>
				</tr>

			
				<tr>
					<td> 10V </td>
					<td> 10.65 </td>
					<td> V </td>
					<td> 0.94 </td>
					<td> 100 </td>
				</tr>

				<tr>
					<td> 10H </td>
					<td> 10.65 </td>
					<td> H </td>
					<td> 0.94 </td>
					<td> 100 </td>
				</tr>

			
				<tr>
					<td> 10R </td>
					<td> 10.65 </td>
					<td> RC </td>
					<td> 0.98 </td>
					<td> 100 </td>
				</tr>

				<tr>
					<td> 10L </td>
					<td> 10.65 </td>
					<td> LC </td>
					<td> 0.98 </td>
					<td> 100 </td>
				</tr>

			
				<tr>
					<td> 18V </td>
					<td> 18.7 </td>
					<td> V </td>
					<td> 1.18 </td>
					<td> 200 </td>
				</tr>

				<tr>
					<td> 18H </td>
					<td> 18.7 </td>
					<td> H </td>
					<td> 1.18 </td>
					<td> 200 </td>
				</tr>

			
				<tr>
					<td> 18P </td>
					<td> 18.7 </td>
					<td> +45 </td>
					<td> 1.18 </td>
					<td> 200 </td>
				</tr>

				<tr>
					<td> 18M  </td>
					<td> 18.7 </td>
					<td> -45 </td>
					<td> 1.18 </td>
					<td> 200 </td>
				</tr>

			
				<tr>
					<td> 18R </td>
					<td> 18.7 </td>
					<td> RC </td>
					<td> 1.32 </td>
					<td> 200 </td>
				</tr>

				<tr>
					<td> 18L </td>
					<td> 18.7 </td>
					<td> LC </td>
					<td> 1.32 </td>
					<td> 200 </td>
				</tr>

			
				<tr>
					<td> 23V </td>
					<td> 23.8 </td>
					<td> V </td>
					<td> 1.0 </td>
					<td> 400 </td>
				</tr>

				<tr>
					<td> 23H </td>
					<td> 23.8 </td>
					<td> H </td>
					<td> 1.0 </td>
					<td> 400 </td>
				</tr>

			
				<tr>
					<td> 36V  </td>
					<td> 36.5 </td>
					<td> V </td>
					<td> 0.63 </td>
					<td> 1000 </td>
				</tr>

				<tr>
					<td> 36H </td>
					<td> 36.5 </td>
					<td> H </td>
					<td> 0.63 </td>
					<td> 1000 </td>
				</tr>

			
				<tr>
					<td> 36P </td>
					<td> 36.5 </td>
					<td> +45 </td>
					<td>  0.63 </td>
					<td> 1000   </td>
				</tr>

				<tr>
					<td> 36M </td>
					<td> 36.5  </td>
					<td> -45 </td>
					<td> 0.63 </td>
					<td> 1000  </td>
				</tr>

				<tr>
					<td> 60VA </td>
					<td> 50.30 </td>
					<td> V </td>
					<td> 2.70 </td>
					<td> 134 </td>
				</tr>

				<tr>
					<td> 60VB </td>
					<td> 52.24 </td>
					<td> V </td>
					<td> 0.92 </td>
					<td> 1280 </td>
				</tr>

				<tr>
					<td> 60VC </td>
					<td> 53.57 </td>
					<td> V </td>
					<td> 1.05 </td>
					<td> 960 </td>
				</tr>

				<tr>
					<td> 60VD </td>
					<td> 54.38 </td>
					<td> V </td>
					<td> 1.51 </td>
					<td> 440 </td>
				</tr>

			
				<tr>
					<td> 60VE </td>
					<td> 54.905 </td>
					<td> V </td>
					<td> 1.69 </td>
					<td> 350 </td>
				</tr>

				<tr>
					<td> 60VF </td>
					<td> 55.49 </td>
					<td> V </td>
					<td> 1.71 </td>
					<td> 340 </td>
				</tr>

			
				<tr>
					<td> 60VG </td>
					<td> 56.66 </td>
					<td> V </td>
					<td> 1.82 </td>
					<td> 300 </td>
				</tr>

				<tr>
					<td> 60VJ </td>
					<td> 59.38 </td>
					<td> V </td>
					<td> 1.88 </td>
					<td> 280 </td>
				</tr>

				<tr>
					<td> 60VK </td>
					<td> 59.94 </td>
					<td> V </td>
					<td> 1.51 </td>
					<td> 440 </td>
				</tr>

				<tr>
					<td> 60LL </td>
					<td> 60.3712 </td>
					<td> LC </td>
					<td> 3.35 </td>
					<td> 57.6 </td>
				</tr>

				<tr>
					<td> 60LM </td>
					<td> 60.408 </td>
					<td> LC </td>
					<td> 6.34 </td>
					<td> 16.0 </td>
				</tr>

				<tr>
					<td> 60LU </td>
					<td> 60.4202 </td>
					<td> LC </td>
					<td> 8.74 </td>
					<td> 8.4 </td>
				</tr>

				<tr>
					<td> 60LV </td>
					<td> 60.5088 </td>
					<td> LC </td>
					<td> 3.80 </td>
					<td> 44.8 </td>
				</tr>

				<tr>
					<td> 60L FFT 1-40 </td>
					<td> 60.43476 </td>
					<td> LC  </td>
					<td> &nbsp; </td>
					<td> 25.0 </td>
				</tr>

				<tr>
					<td> 60L FFT 1, 40 </td>
					<td> &nbsp; </td>
					<td> &nbsp; </td>
					<td> 20.67 </td>
					<td> 1.50 </td>
				</tr>


				<tr>
					<td> 60L FFT 2, 39 </td>
					<td> &nbsp; </td>
					<td> &nbsp; </td>
					<td> 22.65 </td>
					<td> 1.25 </td>
				</tr>

				<tr>
					<td> 60L FFT 3, 4, 37, 38 </td>
					<td> &nbsp; </td>
					<td> &nbsp; </td>
					<td> 25.32 </td>
					<td> 1.00 </td>
				</tr>


				<tr>
					<td> 60L FFT 5, 36 </td>
					<td> &nbsp; </td>
					<td> &nbsp; </td>
					<td> 29.23 </td>
					<td> 0.75 </td>
				</tr>

				<tr>
					<td> 60L FFT 6-8, 33-35 </td>
					<td> &nbsp; </td>
					<td> &nbsp; </td>
					<td> 35.80 </td>
					<td> 0.50 </td>
				</tr>


				<tr>
					<td> 60L FFT 9-32 </td>
					<td> &nbsp; </td>
					<td> &nbsp; </td>
					<td> 50.63 </td>
					<td> 0.25 </td>
				</tr>

				<tr>
					<td> 89V </td>
					<td> 89.0 </td>
					<td> V </td>
					<td> 0.76 </td>
					<td> 4000 </td>
				</tr>


				<tr>
					<td> 89H </td>
					<td> 89.0 </td>
					<td> H </td>
					<td> 0.76 </td>
					<td> 4000 </td>
				</tr>

				<tr>
					<td> 166V </td>
					<td> 166.0 &#177;0.7875 </td>
					<td> V </td>
					<td> 2.37 </td>
					<td> 1425 each </td>
				</tr>


				<tr>
					<td> 183VA </td>
					<td> 183.31&#177;0.7125 </td>
					<td> V </td>
					<td> 2.62 </td>
					<td> 1275 each </td>
				</tr>

				<tr>
					<td> 183VB </td>
					<td> 183.31&#177;3.1 </td>
					<td> V </td>
					<td> 1.61 </td>
					<td> 3500 each </td>
				</tr>

				<tr>
					<td> 183VC </td>
					<td> 183.31&#177;7.7 </td>
					<td> V </td>
					<td> 1.43 </td>
					<td> 4500 each </td>
				</tr>

			</table>
			

			<br><br>


			<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
			<caption>Available Heritage Sensors for CMIS</caption>
				<tr>
					<th scope="col" bgcolor=yellow>Sensor</th>
					<th scope="col" bgcolor=yellow>CMIS</th>
					<th scope="col" bgcolor=yellow>SSMIS</th>
					<th scope="col" bgcolor=yellow>AMSU</th>
					<th scope="col" bgcolor=yellow>WindSat</th>
				</tr>

				<tr>
					<td>Spacecraft </td>
					<td>NPOESS </td>
					<td>DMSP </td>
					<td>NOAA-16 </td>
					<td>Coriolis </td>
				</tr>


				<tr>
					<td>Agency </td>
					<td>NPOESS </td>
					<td>NRL/USAF </td>
					<td>NOAA </td>
					<td>NRL </td>
				</tr>


				<tr>
					<td>Launch Date </td>
					<td>- - </td>
					<td>Oct. 2003 </td>
					<td>Sept. 2001 </td>
					<td>Jan 2003 </td>
				</tr>


				<tr>
					<td>Altitude (km) </td>
					<td>833 </td>
					<td>833 </td>
					<td>850 </td>
					<td>830 </td>
				</tr>


				<tr>
					<td>EIA (deg) </td>
					<td>53.6-58.1</td>
					<td>53.1 </td>
					<td>-57 to 57 </td>
					<td>49.9-55.3 </td>
				</tr>


				<tr>
					<td>Swath (km) </td>
					<td>1700 </td>
					<td>1700 </td>
					<td>2200 </td>
					<td>1025 / 350<br>(Fore / Aft) </td>
				</tr>

			</table>
			
			

			<br><br>


			<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
			<caption>Proxy Data for CMIS Channel Frequencies and Other Polar Orbiting Microwave Radiometers</caption>
				<tr>
					<th scope="col" bgcolor=yellow>CMIS (53-58° EIA)</th>
					<th scope="col" bgcolor=yellow>SSMIS (53° EIA)</th>
					<th scope="col" bgcolor=yellow>AMSU (x-scan)</th>
					<th scope="col" bgcolor=yellow>WindSat (50-55° EIA)</th>
					<th scope="col" bgcolor=yellow>Purpose</th>
				</tr>

				<tr>
					<td>6.625/350/H,V </td>
					<td>&nbsp; </td>
					<td>&nbsp; </td>
					<td>6.8/125/H,V </td>
					<td>Window </td>
				</tr>

				<tr>
					<td>10.65/100/H,V,R,L </td>
					<td>&nbsp; </td>
					<td>&nbsp; </td>
					<td>10.7/300/H,V,+45,-45,R,L </td>
					<td>Window </td>
				</tr>

				<tr>
					<td>18.7/200/H,V,+45,-45,R,L </td>
					<td>&nbsp; </td>
					<td>&nbsp; </td>
					<td>18.7/750/H,V,+45,-45,R,L </td>
					<td>Window </td>
				</tr>

				<tr>
					<td>&nbsp; </td>
					<td>19.35/355,357/H,V </td>
					<td>23.8/251 </td>
					<td>&nbsp; </td>
					<td>Window </td>
				</tr>

				<tr>
					<td>&nbsp; </td>
					<td>22.235/401/V </td>
					<td>31.4/161 </td>
					<td>&nbsp; </td>
					<td>Window </td>
				</tr>

				<tr>
					<td>23.8/400/H,V </td>
					<td>&nbsp; </td>
					<td>&nbsp; </td>
					<td>23.8/500/H,V </td>
					<td>Sounding (H2O) </td>
				</tr>

				<tr>
					<td>36.5/1000/H,V,+45,-45 </td>
					<td>37.0/1545-1615/H,V </td>
					<td>&nbsp; </td>
					<td>37.0/2000/H,V,+45,-45,R,L </td>
					<td>Window </td>
				</tr>

				<tr>
					<td>50.300/134/V </td>
					<td>50.3/380/H </td>
					<td>50.300/161 </td>
					<td>&nbsp; </td>
					<td>Sounding (O2) </td>
				</tr>


				<tr>
					<td>52.240/1280/V </td>
					<td>&nbsp; </td>
					<td>&nbsp; </td>
					<td>&nbsp; </td>
					<td>Sounding (O2)</td>
				</tr>

				<tr>
					<td>&nbsp; </td>
					<td> 52.8/389/H</td>
					<td>52.800/380 </td>
					<td>&nbsp; </td>
					<td>Sounding (O2) </td>
				</tr>

				<tr>
					<td>53.570/960/V </td>
					<td>53.596/380/H </td>
					<td>53.600/168 </td>
					<td>&nbsp; </td>
					<td>Sounding (O2) </td>
				</tr>

				<tr>
					<td>54.380/440/V </td>
					<td>54.4/383/H </td>
					<td>54.400/380 </td>
					<td>&nbsp; </td>
					<td>Sounding (O2) </td>
				</tr>

				<tr>
					<td>54.905/350/V </td>
					<td>&nbsp; </td>
					<td>54.940/380 </td>
					<td>&nbsp; </td>
					<td>Sounding (O2) </td>
				</tr>

				<tr>
					<td>55.490/340/V </td>
					<td>55.5/391/H </td>
					<td>55.500/310 </td>
					<td>&nbsp; </td>
					<td>Sounding (O2) </td>
				</tr>

				<tr>
					<td>56.660/300/V </td>
					<td>&nbsp; </td>
					<td>&nbsp; </td>
					<td>&nbsp; </td>
					<td>Sounding (O2) </td>
				</tr>

				<tr>
					<td>&nbsp; </td>
					<td>57.29/330/R </td>
					<td>fo = 57.290/310 </td>
					<td>&nbsp; </td>
					<td>Sounding (O2) </td>
				</tr>

				<tr>
					<td>59.380/280/V </td>
					<td>59.4/239/R </td>
					<td>fo &#177; 0.217/76 </td>
					<td>&nbsp; </td>
					<td>Sounding (O2) </td>
				</tr>

				<tr>
					<td>59.940/440/V </td>
					<td>&nbsp; </td>
					<td>fo &#177; 0.322 &#177; 0.048/34 </td>
					<td>&nbsp; </td>
					<td>Sounding (O2) </td>
				</tr>

				<tr>
					<td>60.371200/57.6/L </td>
					<td>&nbsp; </td>
					<td>fo &#177; 0.322 &#177; 0.022/15 </td>
					<td>&nbsp; </td>
					<td>Sounding (O2) </td>
				</tr>

				<tr>
					<td>60.4080/16.0/L </td>
					<td>&nbsp; </td>
					<td>fo &#177; 0.322 &#177; 0.010/8 </td>
					<td>&nbsp; </td>
					<td>Sounding (O2) </td>
				</tr>

				<tr>
					<td>60.4202/8.4/L </td>
					<td>&nbsp; </td>
					<td>fo &#177; 0.322 &#177; 0.004/3 </td>
					<td>&nbsp; </td>
					<td>Sounding (O2) </td>
				</tr>

				<tr>
					<td>60.5088/44.8/L </td>
					<td>&nbsp; </td>
					<td>&nbsp; </td>
					<td>&nbsp; </td>
					<td>Sounding (O2) </td>
				</tr>

				<tr>
					<td>60.434776/25.0, FFT 40 channels/L </td>
					<td>&nbsp; </td>
					<td>&nbsp; </td>
					<td>&nbsp; </td>
					<td>Sounding (O2) </td>
				</tr>

				<tr>
					<td>&nbsp; </td>
					<td colspan=2>60.792668&#177;0.357892&#177; /1.3-26.5/R (5 channels) </td>
					<td>&nbsp; </td>
					<td>Sounding (O2) </td>
				</tr>


				<tr>
					<td>&nbsp;  </td>
					<td>63.283248&#177;0.285271/1.35/R </td>
					<td>&nbsp;  </td>
					<td>&nbsp;  </td>
					<td>Sounding (O2) </td>
				</tr>

				<tr>
					<td>89.0/4000/H,V </td>
					<td>&nbsp; </td>
					<td>89.0/3000 </td>
					<td>&nbsp; </td>
					<td>Window </td>
				</tr>

				<tr>
					<td>&nbsp; </td>
					<td>91.655 /1411-1418/H,V </td>
					<td>&nbsp; </td>
					<td>&nbsp; </td>
					<td>Window </td>
				</tr>

				<tr>
					<td>&nbsp; </td>
					<td>150.0 /1642/H </td>
					<td>150 /4000 </td>
					<td>&nbsp; </td>
					<td>Sounding (H2O) </td>
				</tr>

				<tr>
					<td>166.0&#177;0.7875/1425/V </td>
					<td>&nbsp; </td>
					<td>&nbsp; </td>
					<td>&nbsp; </td>
					<td>Sounding (H2O) </td>
				</tr>

				<tr>
					<td>183.31&#177;0.7125/1275/V </td>
					<td>183.31&#177;1.0/513/H </td>
					<td>183.31&#177;1.0/1000 </td>
					<td>&nbsp; </td>
					<td>Sounding (H2O) </td>
				</tr>

				<tr>
					<td>183.31&#177;3.1/3500/V </td>
					<td>183.31&#177;3.0/1019/H </td>
					<td>183.31&#177;3.0/2000 </td>
					<td>&nbsp; </td>
					<td>Sounding (H2O) </td>
				</tr>

				<tr>
					<td>183.31&#177;7.7/4500/V </td>
					<td>183.31&#177;6.6/1526/H </td>
					<td>183.31&#177;7.0/4000 </td>
					<td>&nbsp; </td>
					<td>Sounding (H2O) </td>
				</tr>

			</table>
			
			<br><br>
			<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
			
				<tr>
					<th scope="col" bgcolor=yellow>Key</th>
					<th scope="col" bgcolor=yellow>&nbsp;</th>
					<th scope="col" bgcolor=yellow>&nbsp;</th>
				</tr>


				<tr>
					<td>XXX/YYY/V,H,+45,-45, L,R </td>
					<td>Center Freq. (GHz)/ Bandwidth  (MHz)/ Polarization </td>
					<td>Window channel match </td>
				</tr>

				<tr>
					<td>V, H, +45, -45, L, R </td>
					<td>Vertical, Horizontal, +45°, -45°, Left Circular,<br> Right Circular Polarization </td>
					<td> Sounding channel match</td>
				</tr>

				<tr>
					<td>EIA </td>
					<td>Earth Incidence Angle </td>
					<td>&nbsp; </td>
				</tr>
			
			</table>
			
			<br>
			<p>*The configuration provided here is based on the former CMIS instrument which was 
			planned to fly on NPOESS but was canceled from the NPOESS project.
			Its successor, the MIS sensor is at this point in time, not clearly 
			defined in terms of sensor characteristics.</p>





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
