<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>STAR - Microwave Integrated Retrieval System (MIRS) - Sensors - SAPHIR</title>
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
			Megha-Tropiques SAPHIR Overview</h1>

			<p>
			SAPHIR is a sounding instrument with 6 channels near the absorption band of water vapor at 183 Ghz. 
			These channels provide relatively narrow weighting functions from the surface to about 10 km, 
			allowing retrieving water vapor profiles in the cloud free troposphere. The scanning is cross-track, 
			up to an incidence angle of 50°. The resolution at nadir is of 10 km.
			</p>


			
			<table border="0" cellpadding="0" cellspacing="0" class="tableGrid">
			<caption> SAPHIR channel specification </caption>
				
				<tr>
					<th scope="col" >Channel</th>
					<th scope="col" >Central Frequency (GHz)</th>
					<th scope="col" >Bandwidth (MHz)</th>
					<th scope="col" >NE&#8710;T </th>
					<th scope="col" >Polarization</th>
					<th scope="col" >Interchannel calibration </th>

				</tr>

				<tr>
					<th scope="row">S1</th>
					<td align=center>183.31&#177;0.20</td> 
					<td align=center>200</td>
					<td align=center>2.35 K</td>
					<td align=center>H</td> 
					<td align=center>0.5K</td>

				</tr>

				<tr>
					<th scope="row">S2</th>
					<td align=center>183.31&#177;1.10</td> 
					<td align=center>350</td> 
					<td align=center>1.45 K</td>
					<td align=center>H</td> 
					<td align=center>0.5K</td>

				</tr>

				<tr>
					<th scope="row">S3</th>
					<td align=center>183.31&#177;2.80</td> 
					<td align=center>500</td>
					<td align=center>1.36 K</td>
					<td align=center>H</td> 
					<td align=center>0.5K</td>

				</tr>

				<tr>
					<th scope="row">S4</th>
					<td align=center>183.31&#177;4.20</td> 
					<td align=center>700</td> 
					<td align=center>1.38 K</td>
					<td align=center>H</td> 
					<td align=center>0.5K</td>

				</tr>

				<tr>
					<th scope="row">S5</th>
					<td align=center>183.31&#177;6.80</td> 
					<td align=center>1200</td>
					<td align=center>1.03 K</td>
					<td align=center>H</td> 
					<td align=center>0.5K</td>

				</tr>

				<tr>
					<th scope="row">S6</th>
					<td align=center>183.31&#177;11.0</td> 
					<td align=center>200</td> 
					<td align=center>1.10 K</td>
					<td align=center>H</td> 
					<td align=center>0.5K</td>

				</tr>


				</table>
				<br />

			<br />
			
			<hr><br />
			<center><H3><font color="#282888">Scan pattern of the SAPHIR instrument (image credit: CNES) </font></H3> </center>
			<img src="instruments/Megha_Auto1.jpg"><br /><br />


			<hr><br />
			<center><H3><font color="#282888">The 6 channels of SAPHIR positioned versus the water vapour absorption line at 183.31 Ghz </font></H3> </center>
			<img src="instruments/SAPHIR.jpg"><br />
			
			
		
				<br />
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
