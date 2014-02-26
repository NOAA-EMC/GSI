<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - Microwave Integrated Retrieval System (MIRS) - Sensors - MADRAS</title>
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
			Megha-Tropiques MADRAS Overview</h1>

			<p>
			MADRAS is a microwave imager, with conical scanning (incidence angle 56°), 
			close from the SSM/I and TMI concepts. The main aim of the mission being 
			the study of cloud systems, a frequency has been added (157 Ghz) in order 
			to study the high level ice clouds associated with the convective systems, 
			and to serve as a window channel relative to the sounding instrument at 183 GHz.
			</p>
			
			<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
			<caption> main characteristics of the MADRAS channels </caption>
				
				<tr>
					<th scope="col" >Channel Number</th>
					<th scope="col" >Frequency</th>
					<th scope="col" >Polarization</th>
					<th scope="col" >NE&#8710;T @ 300K required, (goal) </th>
					<th scope="col" >Pixel size</th>
					<th scope="col" >Main use</th>
				</tr>

				<tr>
					<th scope="row">1</th>
					<td align=center>18.7 Ghz &#177; 100 Mhz</td> 
					<td align=center>V</td>
					<td align=center>0.7 K (0.5)</td>
					<td align=center>40 km</td> 
					<td align=center>ocean rain and surface wind</td>

				</tr>

				<tr>
					<th scope="row">2</th>
					<td align=center>18.7 Ghz &#177; 100 Mhz</td> 
					<td align=center>H</td> 
					<td align=center>0.7 K (0.5)</td>
					<td align=center>40 km</td> 
					<td align=center>ocean rain and surface wind</td>

				</tr>

				<tr>
					<th scope="row">3</th>
					<td align=center>23.8 Ghz &#177; 200 Mhz</td>
					<td align=center>V</td>
					<td align=center>0.7 K (0.5)</td>
					<td align=center>40 km</td>
					<td align=center>integrated water vapor</td>
				</tr>

				<tr>
					<th scope="row">4</th>
					<td align=center>36.5 Ghz &#177; 500 Mhz</td>
					<td align=center>V</td>
					<td align=center>0.7 K (0.5)</td>
					<td align=center>40 km</td>
					<td align=center>cloud liquid water</td>
				</tr>

				<tr>
					<th scope="row">5</th>
					<td align=center>36.5 Ghz &#177; 500 Mhz</td>
					<td align=center>H</td>
					<td align=center>0.7 K (0.5)</td>
					<td align=center>40 km</td>
					<td align=center>cloud liquid water</td>
				</tr>

				<tr>
					<th scope="row">6</th>
					<td align=center>89 Ghz &#177; 1350 Mhz</td>
					<td align=center>V</td>
					<td align=center>1.1 K (1.0)</td>
					<td align=center>10 km</td>
					<td align=center>convective rain areas</td>
				</tr>

				<tr>
					<th scope="row">7</th>
					<td align=center>89 Ghz &#177; 1350 Mhz</td>
					<td align=center>H</td>
					<td align=center>1.1 K (1.0)</td>
					<td align=center>10 km</td>
					<td align=center>convective rain areas</td>
				</tr>

				<tr>
					<th scope="row">8</th>
					<td align=center>157 Ghz &#177; 1350 Mhz</td>
					<td align=center>V</td>
					<td align=center>2.2 K (2.0)</td>
					<td align=center>6 km</td>
					<td align=center>cloud top ice</td>
				</tr>

				<tr>
					<th scope="row">9</th>
					<td align=center>157 Ghz &#177; 1350 Mhz</td>
					<td align=center>H</td>
					<td align=center>2.2 K (2.0)</td>
					<td align=center>6 km</td>
					<td align=center>cloud top ice</td>
				</tr>


				</table>
				<br />
				
				<p>
				The main uses given here are only indicative, most of the products being extracted 
				from algorithms combining the different channels information. The resolutions 
				are those expected in the different channels, accounting for the specification 
				of 10 km given for the 89 Ghz channel.
				</p>
				
				<ul>
					<li><strong>V:</strong> Polarization vector is parallel to scan plane at nadir</li>
					<li><strong>H:</strong> Polarization vector is perpendicular to scan plane at nadir</li>
				</ul>
				<br />
				
				
				<hr><br />
				<center><H3><font color="#282888">Imaging geometry of the Megha-Tropiques instruments (image credit: CNES)</font></H3> </center>
				<img src="instruments/Megha_Auto6.jpg"><br /><br />

				
				
				
				<hr>
				<br />
					<p>Source: <a href="http://meghatropiques.ipsl.polytechnique.fr/instruments.html"> MEGHA-TROPIQUES Home</a></p>
					<p>Source: <a href="http://www.eoportal.org/directory/pres_MeghaTropiquesMeteorologicalLEOObservationsintheIntertropicalZone.html"> MEGHA-TROPIQUES @ eoPortal</a></p>

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
